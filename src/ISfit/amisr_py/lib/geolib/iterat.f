C     $Id: iterat.f,v 1.2 2001/03/14 22:46:04 sjc Exp $
C
      SUBROUTINE ITERAT
C
C     Private/Internal subroutine. Part of Apex coordinate computation
C     package. See COORD for public API. ITERAT integrates magnetic
C     field line using a 4-point adams formula after initialization.
C     First 7 iterations advance point by 3*DS.
C
C       Input (via common block ITER):
C               L - step count. set l=1 first time thru,
C                   set l=l+1 thereafter.
C      B,BR,BT,BP - field + components at point y
C              ST - sine of geocentric colatitude
C             SGN - sgn=+1  traces in direction of field
C                   sgn=-1  traces in negative field direction
C              DS - integration stepsize (arc increment) in km
C
C      Input, Output (via common block ITER):
C               Y - R,DLAT,DLON: geocentric tracing point coordinates 
C                   (km,deg)
C
C      Output (via common block ITER):
C            YOLD - Y at iteration L-1
C
C     .. Scalars in Common ..
C     ..
C     .. Arrays in Common ..
C     ..
C     .. Local Scalars ..
C
      DOUBLE PRECISION D12,D2,D24,D6,FAC,RAD
      INTEGER I,J
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION YP(3,4)
C     ..
C     .. Common blocks ..
      COMMON /ITER/Y,YOLD,HP,BR,BT,BP,B,ST,SGN,DS,L
      DOUBLE PRECISION B,BP,BR,BT,DS,HP,SGN,ST
      INTEGER L
      DOUBLE PRECISION Y(3),YOLD(3)
C     ..
C     .. Save statement ..
      SAVE D12,D2,D24,D6,FAC,RAD,I,J,YP
C     ..
C     .. Data statements ..
      DATA RAD/57.2957795D0/,D12/0.0D0/,D2/0.0D0/,D24/0.0D0/,D6/0.0D0/
C     ..
C
      YP(1,4) = SGN*BR/B
      FAC = SGN*RAD/(B*Y(1))
      YP(2,4) = -BT*FAC
      YP(3,4) = BP*FAC/ST
      IF (L.GT.7) THEN
         DO 20 I = 1,3
            YOLD(I) = Y(I)
            Y(I) = YOLD(I) + D24*(55.D0*YP(I,4)-59.D0*YP(I,3)+
     *             37.D0*YP(I,2)-9.D0*YP(I,1))
            DO 10 J = 1,3
               YP(I,J) = YP(I,J+1)
   10       CONTINUE
   20    CONTINUE
      ELSE
         DO 30 I = 1,3
            IF (L.EQ.2) THEN
               YP(I,2) = YP(I,4)
               Y(I) = YOLD(I) + D2*(YP(I,2)+YP(I,1))
            ELSE IF (L.EQ.3) THEN
               Y(I) = YOLD(I) + D6*(2.D0*YP(I,4)+YP(I,2)+3.D0*YP(I,1))
            ELSE IF (L.EQ.4) THEN
               YP(I,2) = YP(I,4)
               YOLD(I) = Y(I)
               Y(I) = YOLD(I) + D2*(3.D0*YP(I,2)-YP(I,1))
            ELSE IF (L.EQ.5) THEN
               Y(I) = YOLD(I) + D12*(5.D0*YP(I,4)+8.D0*YP(I,2)-YP(I,1))
            ELSE IF (L.EQ.6) THEN
               YP(I,3) = YP(I,4)
               YOLD(I) = Y(I)
               Y(I) = YOLD(I) + D12*(23.D0*YP(I,3)-16.D0*YP(I,2)+
     *                5.D0*YP(I,1))
            ELSE IF (L.EQ.7) THEN
               Y(I) = YOLD(I) + D24*(9.D0*YP(I,4)+19.D0*YP(I,3)-
     *                5.D0*YP(I,2)+YP(I,1))
            ELSE
               D2 = DS/2.D0
               D6 = DS/6.D0
               D12 = DS/12.D0
               D24 = DS/24.D0
               YP(I,1) = YP(I,4)
               YOLD(I) = Y(I)
               Y(I) = YOLD(I) + DS*YP(I,1)
            END IF
   30    CONTINUE
      END IF
      RETURN
C
      END
