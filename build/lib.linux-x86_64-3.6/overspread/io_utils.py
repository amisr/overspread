#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""
import scipy
import tables
import os

def read_whole_h5file(fname):

    h5file=tables.open_file(fname)
    output={}
    for group in h5file.walk_groups("/"):
        output[group._v_pathname]={}
        for array in h5file.list_nodes(group, classname = 'Array'):
            output[group._v_pathname][array.name]=array.read()
    h5file.close()

    return output

def ini_tool(config,secName,parmName,required=0,defaultParm=''):

    try:
        if config.has_option(secName,parmName):
            parm=config.get(secName,parmName)
        elif required==1:
            raise IOError('%s must have parameter %s' % (secName,parmName))
        else:
            parm=defaultParm
    except:
        raise IOError('Error reading %s from %s' % (parmName,secName))

    return parm

# loads the lag ambiguity function from a file
def load_amb_func(path,full=0):

    outDict={}

    dat=read_whole_h5file(path)
    outDict=dat['/']

    return outDict

def copyAmbDict(inAmb):
    # Copies ambiguity function from data files to something the fitter expects and needs

    outAmb={}
    try:
        outAmb['Delay']=inAmb['Delay']
        outAmb['Range']=inAmb['Range']
        outAmb['Lags']=inAmb['Lags']
        outAmb['Wlag']=inAmb['Wlag']
        outAmb['Wrange']=inAmb['Wrange']
        if 'Bandwidth' in inAmb.keys():
            outAmb['Bandwidth']=inAmb['Bandwidth']
    except:
        print('Dont understand format of Ambiguity in data file.')

    return outAmb

def write_outputfile(fhandle,dict2do,keys2do=[],groupname='',name='',grouploc='/'):
    # Tested with pytables 2.0.1

    if groupname == '':
        group=fhandle.root
    else:
        if fhandle.__contains__(grouploc+groupname):
            group='/'+groupname
        else:
			#fhandle.root
            group=fhandle.create_group(grouploc, groupname, 'Dataset')

    if len(keys2do)==0:
        try:
            fhandle.remove_node(group,name)
        except:
            ''
        if isinstance(dict2do,str):
            dict2do = scipy.array(dict2do)
        fhandle.create_array(group,name, dict2do, "Dataset")
    else:
        for key in keys2do:
            if type(dict2do[key]) is dict:
                write_outputfile(fhandle,dict2do[key],keys2do=dict2do[key].keys(),groupname=key,grouploc='/'+group._v_name)
            else:
                fhandle.create_array(group, key, dict2do[key], "Dataset")

    return

def createh5groups(fhandle,h5Groups):
    # creates groups
    for group in h5Groups:
        gp,gn = os.path.split(group[0])
        fhandle.create_group(gp,gn,group[1])
    return

def createStaticArray(fhandle,path,data,keys2do=[]):
    # creates a static array
    if len(keys2do) == 0:
        dp,dn = os.path.split(path)
        if isinstance(data,str):
            dat = scipy.array(data)
        else:
            dat = data
        fhandle.create_array(dp,dn,dat,'Static array')
    else:
        for key in keys2do:
            if isinstance(data[key],str):
                dat = scipy.array(data[key])
            else:
                dat = data[key]
            fhandle.create_array(path,key,dat,'Static array')
    return

def createDynamicArray(fhandle,path,rec,keys2do=[]):
    # creates a dynamic array
    if len(keys2do)==0:
        dp,dn = os.path.split(path)
        data = rec.copy()
        data.shape = (1,)+data.shape  ## add integration dimension to data array
        if not fhandle.__contains__(path):
            shape = list(data.shape)
            shape[0] = 0
            atom = tables.Atom.from_dtype(data.dtype)
            arr = fhandle.create_earray(dp,dn,atom,shape)
            arr.flavor='numpy'
        arr = fhandle.get_node(path)
        if (len(arr.shape)>2) and (data.shape[2] != arr.shape[2]):
            if data.shape[2] > arr.shape[2]:
                # read array
                tarr = arr.read()
                # remove old node
                arr.remove()
                tshape=list(tarr.shape); tshape[2]=data.shape[2]-tarr.shape[2]
                tarr=scipy.append(tarr,scipy.zeros(tshape)*scipy.nan,axis=2)
                # create new node
                shape = list(tarr.shape)
                shape[0] = 0
                atom = tables.Atom.from_dtype(tarr.dtype)
                arr = fhandle.create_earray(dp,dn,atom,shape)
                arr.flavor='numpy'
                arr = fhandle.get_node(path)
                # dump data
                arr.append(tarr)
            else:
                tshape = list(data.shape); tshape[2]=arr.shape[2]-data.shape[2]
                data = scipy.append(data,scipy.zeros(tshape)*scipy.nan,axis=2)
        arr.append(data)
        arr.flush()
    else:
        for key in keys2do:
            data = scipy.array(rec[key])
            data.shape = (1,)+data.shape  ## add integration dimension to data array
            if not fhandle.__contains__(path+'/'+key):
                shape = list(data.shape)
                shape[0] = 0
                atom = tables.Atom.from_dtype(data.dtype)
                arr = fhandle.create_earray(path,key,atom,shape)
                arr.flavor='numpy'
            arr = fhandle.get_node(path+'/'+key)
            if (len(arr.shape)>2) and (data.shape[2] != arr.shape[2]):
                if data.shape[2] > arr.shape[2]:
                    # read array
                    tarr = arr.read()
                    # remove old node
                    arr.remove()
                    tshape=list(tarr.shape); tshape[2]=data.shape[2]-tarr.shape[2]
                    tarr=scipy.append(tarr,scipy.zeros(tshape)*scipy.nan,axis=2)
                    # create new node
                    shape = list(tarr.shape)
                    shape[0] = 0
                    atom = tables.Atom.from_dtype(tarr.dtype)
                    arr = fhandle.create_earray(path,key,atom,shape)
                    arr.flavor='numpy'
                    arr = fhandle.get_node(path+'/'+key)
                    # dump data
                    arr.append(tarr)
                else:
                    tshape=list(data.shape); tshape[2]=arr.shape[2]-data.shape[2]
                    data=scipy.append(data,scipy.zeros(tshape)*scipy.nan,axis=2)
            arr.append(data)
            arr.flush()
    return

def setAtrributes(fhandle,data):
    for key in data.keys():
        for attr in data[key]:
            try:  fhandle.set_node_attr(key,attr[0],attr[1])
            except: ''
    return


def createDynamicArray2(fhandle,path,rec,keys2do=[]):
    # creates a dynamic array
    if len(keys2do)==0:
        dp,dn = os.path.split(path)
        data = rec.copy()
        #data.shape = (1,)+data.shape  ## add integration dimension to data array
        if not fhandle.__contains__(path):
            shape = list(data.shape)
            shape[0] = 0
            atom = tables.Atom.from_dtype(data.dtype)
            arr = fhandle.create_earray(dp,dn,atom,shape)
            arr.flavor='numpy'
        arr = fhandle.get_node(path)
        if (len(arr.shape)>2) and (data.shape[2] != arr.shape[2]):
            if data.shape[2] > arr.shape[2]:
                # read array
                tarr = arr.read()
                # remove old node
                arr.remove()
                tshape=list(tarr.shape); tshape[2]=data.shape[2]-tarr.shape[2]
                tarr=scipy.append(tarr,scipy.zeros(tshape)*scipy.nan,axis=2)
                # create new node
                shape = list(tarr.shape)
                shape[0] = 0
                atom = tables.Atom.from_dtype(tarr.dtype)
                arr = fhandle.create_earray(dp,dn,atom,shape)
                arr.flavor='numpy'
                arr = fhandle.get_node(path)
                # dump data
                arr.append(tarr)
            else:
                tshape=list(data.shape); tshape[2]=arr.shape[2]-data.shape[2]
                data=scipy.append(data,scipy.zeros(tshape)*scipy.nan,axis=2)
        arr.append(data)
        arr.flush()
    else:
        for key in keys2do:
            data = scipy.array(rec[key])
            #data.shape = (1,)+data.shape  ## add integration dimension to data array
            if not fhandle.__contains__(path+'/'+key):
                shape = list(data.shape)
                shape[0] = 0
                atom = tables.Atom.from_dtype(data.dtype)
                arr = fhandle.create_earray(path,key,atom,shape)
                arr.flavor='numpy'
            arr = fhandle.get_node(path+'/'+key)
            if (len(arr.shape)>2) and (data.shape[2] != arr.shape[2]):
                if data.shape[2] > arr.shape[2]:
                    # read array
                    tarr = arr.read()
                    # remove old node
                    arr.remove()
                    tshape=list(tarr.shape); tshape[2]=data.shape[2]-tarr.shape[2]
                    tarr=scipy.append(tarr,scipy.zeros(tshape)*scipy.nan,axis=2)
                    # create new node
                    shape = list(tarr.shape)
                    shape[0] = 0
                    atom = tables.Atom.from_dtype(tarr.dtype)
                    arr = fhandle.create_earray(path,key,atom,shape)
                    arr.flavor='numpy'
                    arr = fhandle.get_node(path+'/'+key)
                    # dump data
                    arr.append(tarr)
                else:
                    tshape=list(data.shape); tshape[2]=arr.shape[2]-data.shape[2]
                    data=scipy.append(data,scipy.zeros(tshape)*scipy.nan,axis=2)
            arr.append(data)
            arr.flush()
    return
