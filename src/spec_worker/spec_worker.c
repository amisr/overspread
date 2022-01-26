/*
MLspec.c
Plasma spectra calculations
Markku Lehtinen 07/1995
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

/* #include "mex.h" */

/* void pldfas(double *retR,double *retI,double zR,double zI);
void adminvec(long nom,double *pldfvPr,double *pldfvPi,double *apuprv,double *apupiv,double *omv,double vi,double psi,double gam); */

/*Asymtotic expansion of plasma dispersion function to be used for abs(z)>3.5*/
void pldfas(double *retR,double *retI,double zR,double zI)
{
	register double termR,termI,ckfR,ckfI,resI,resR,zRR,zII,izRRpII,izRRpII2;
	double termRold;
	
	zRR=zR*zR; zII=zI*zI;
	izRRpII= 1.0 / (zRR+zII);
	izRRpII2=izRRpII*izRRpII;
	ckfR = 0.5*(zRR - zII)*izRRpII2;
	ckfI = -zR*zI*izRRpII2;

	termR = ckfR;
	termI = ckfI;
	resR = 1.0 + termR;
	resI =  termI;

	termR = termR*3.0;
	termI = termI*3.0;
	termRold = termR;
	termR = termR*ckfR - termI*ckfI;
	termI = termI*ckfR + termRold*ckfI;
	resR = resR + termR;
	resI = resI + termI;

/*if more accuracy is desired, add the following 
   		(and then seven similar lines with *7.0 instead of *5.0 etc .... )
	termR = termR*5.0;
	termI = termI*5.0;
	termRold = termR;
	termR = termR*ckfR - termI*ckfI;
	termI = termI*ckfR + termRold*ckfI;
	resR = resR + termR;
	resI = resI + termI; */

	*retR = (resR*zR + resI*zI)*izRRpII;
	*retI = (zR*resI - resR*zI)*izRRpII;
}

/* 
Admittance calculation for each flake 
input parameters:
nom: number of frequencies in omega-axis
pldfvPr, pldfvPi : pointers (real and imaginary) to 4-degree Lagrange
                   interpolation polynomial coefficient table of pldf.
				   Polynomial coeffiecients are presented in the form
				     pldf(z)=((a0*z+a1)*z+a2)*z+a3
omv: Spectrum axis input.
vi: normalized speed of flake
psi: normalized collision frequency of flake
gam: normalized scale depending on temperature of flake. 
output:
apuprv, apupiv : Admittance result real and imaginary part vectors.
				 We store values that are independent of Ni for each
				 flake to avoid recalculation in certain cases.
*/

void adminvec(long nom,double *pldfvPr,double *pldfvPi,double *apuprv,double *apupiv,double *omv,double vi,double psi,double gam)
{
	double x,y,resRo;
	double pR,pI;
	double resR,resI;
	register long k;
	long ix,ffx,ffy,i1,i2,i3;
	register double apupr,apupi;
	register double apusqr;
	register double apuuapr;
	double zR;

	/* (negative collision frequencies need not be handled correctly */	
	if (psi <= -0.03125) { psi = -0.03125; } 
	/* pldf is tabulated at 1.0/16.0 intervals, normalize arguments to that 
	   and calculate integer anf fractional parts :*/
	y = psi*16.0;
	ffy=(long)(y+0.5);
	pI=-(y-ffy); 
	for (k=0;k<nom;k++) {
		zR = (omv[k]-vi)*gam;
		if (zR<0) {x = -zR*16.0;} else {x = zR*16.0;} /* use symmetry */
		if (  (x >= (3.9*16.0) ) || ( psi >= 3.5 ) ) { /* asymptotic formula */
			pldfas(&resR,&resI,zR,-psi); 
		} else { 
			/* plasma dispersion function of z=zR+i*zI is calculated in
			   this loop */
			ffx=(long)x;
			pR=x-ffx;
			ix=(4*ffx+256*ffy);  

			i1=ix+1;i2=ix+2;i3=ix+3;

			/* calculate the complex polynomial: */
			resR = pldfvPr[ix] * pR - pldfvPi[ix] * pI + pldfvPr[i1];
			resI = pldfvPr[ix] * pI + pldfvPi[ix] * pR + pldfvPi[i1];
			resRo = resR * pR - resI * pI + pldfvPr[i2];
			resI = resR * pI + resI * pR + pldfvPi[i2];
			resR = resRo * pR - resI * pI + pldfvPr[i3];
			resI = resRo * pI + resI * pR + pldfvPi[i3];
			
			if (zR<0) { resR = -resR;} /* pldf is conjugate symmetric */
/*			flops += 26;*/
		} 
		/* at last, calculate admittance expressions as long as can be done
		   without introducing Ni dependence : */
		apupr = 1.0 - psi*resI;
		apupi = -psi*resR;
		apusqr =  1.0 / (apupr*apupr + apupi*apupi);
		apuuapr = apupr;
		apuprv[k] = (resI*apupr  +  apupi * resR)*apusqr;
		apupiv[k] = (resI*apupi  -  apuuapr * resR)*apusqr;
	}
}

void specCalc(double *pldfvPr,double *pldfvPi,double *nin0Pr,double *tit0Pr,long nion,double *mim0Pr,double *psiPr,double *viPr,double kd2Pr,double *scr,long nom,double *omPr,double *resPr,long ifref)
{
	unsigned long i,k,maxflakes=nion+2; /*maxflakes=maximum number of flakes(=ions+electrons+one scratch) */
	double uar;
	double yar, yai;
	double yars, yais;
	double uars;
	double apupr,apupi;
	double gamPr[9],gamnPr[9],npertPr[9]; /*9=>max nion=7*/
	double *gamref,*psiref,*viref;
	double *apuprv,*apupiv,*apuprvref,*apupivref;
	double zR;
	
/*	FILE *f1;

	f1 = fopen("loglog", "w");
	for (i=0; i<=1; i++) {
		fprintf(f1,"%f %f %f\n",nin0Pr[i],tit0Pr[i],mim0Pr[i]);
	}	
	for (i=0; i<500; i++) {
		fprintf(f1,"%f %f\n",pldfvPr[i],pldfvPi[i]);
	}
	fclose(f1);*/
	
	/* For reference spectra, the normalized parameters affecting stored admittances 
	   are kept in scr. These values need to be preserved between successive calls */
	psiref=&scr[0];
	viref=&scr[1*maxflakes];
	gamref=&scr[2*maxflakes];
	/* use scr also for temporary storage for admittances. Maybe some speed is
	   gained by avoiding mallocs this way */
	apuprv=&scr[3*maxflakes];
	apupiv=&scr[3*maxflakes+maxflakes*nom];
	/* admittances for reference spectra are also stored in scr. These vectors
	   need to be preserved between successive calls */
	apuprvref=&scr[3*maxflakes+2*maxflakes*nom];
	apupivref=&scr[3*maxflakes+3*maxflakes*nom];
	
	for(i=0;i<(nion+1);i++) 
		{
		gamPr[i] = (float)sqrt( (float)(mim0Pr[i]/tit0Pr[i]) ); 
		npertPr[i] = (nin0Pr[i]/tit0Pr[i]); 
		gamnPr[i] = gamPr[i] * nin0Pr[i]; 
		/* test if parameters effecting admittance vector has changed and in that case recalculate */
		if ( (psiref[i] != psiPr[i]) || (viref[i] != viPr[i]) || (gamref[i] != gamPr[i]) ) {
			adminvec(nom,pldfvPr,pldfvPi,&apuprv[i*nom],&apupiv[i*nom],
				omPr,viPr[i],psiPr[i],gamPr[i]);
		} else { /* just read previuosly calculated admittance from scratch memory */
			for (k=0;k<nom;k++) {
				apuprv[i*nom+k]=apuprvref[i*nom+k];
				apupiv[i*nom+k]=apupivref[i*nom+k];
			}
		}
		if (ifref == 1) { /* if reference spectra is specified, store admittances 
		                     and parameters affecting them for later use */
			psiref[i] = psiPr[i];
			viref[i] = viPr[i];
			gamref[i] = gamPr[i];
			for (k=0;k<nom;k++) {
				apuprvref[i*nom+k]=apuprv[i*nom+k];
				apupivref[i*nom+k]=apupiv[i*nom+k];
			}
		}
	}

	for(k=0;k<nom;k++) {
		/* calculate final admittances from almost ready values in apuprv+i*apupiv */
		/* sum of ion admittances: */
		yais=kd2Pr; yars=0.; uars=0.;
		for(i=0;i<nion;i++){
			zR=(omPr[k]-viPr[i])*gamPr[i];
			yais+=(1.0+zR*apupiv[k+i*nom])*npertPr[i];
			yars+=apuprv[k+i*nom]*zR*npertPr[i]; 
			uars+=apuprv[k+i*nom]*gamnPr[i];
		}
		/* electron admittance: */
		zR=(omPr[k]-viPr[nion])*gamPr[nion];
		yai= (1.0+zR*apupiv[k+nion*nom])*npertPr[nion];
		yar= apuprv[k+nion*nom]*zR*npertPr[nion];
		uar= apuprv[k+nion*nom]*gamnPr[nion];
		/* sum of all admittances */
		apupr = yar + yars;
		apupi = yai + yais;

		/* final spectrum calculation */
		resPr[k]=((yar*yar+yai*yai)*uars+(yars*yars+yais*yais)*uar)
			/((apupr*apupr+apupi*apupi)*3.1415926535897932385);
	}
}
