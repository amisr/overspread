import os
import sys
from PyQt4 import QtGui
from PyQt4.QtCore import pyqtSlot
from fitting import *

class GUI(QtGui.QMainWindow):

    def __init__(self,config):
        self.config = config

    def get_dirs(self):

        # initialize application
        a = QtGui.QApplication(sys.argv)

        # The QWidget widget is the base class of all user interface objects in PyQt4.
        w = QtGui.QWidget()

        # initialize variables
        w.LP = False
        w.AC = False
        w.processing_dirs = []
        w.cal_file = []

        # window title
        w.setWindowTitle("Super *AWESOME* GUI by Steve and Emma")

        # window size
        w.resize(600, 390)

        # button to choose processing folders
        btn_add = QtGui.QPushButton('Add folders for processing', w)
        btn_add.move(10,0)

        # button to delete processing folders
        btn_rm = QtGui.QPushButton('Remove folders for processing', w)
        btn_rm.move(300,0)

        #Listbox for processing folders
        lista = QtGui.QListWidget(w)
        lista.move(10,30)
        lista.resize(580,200)

        # button for adding calibration file
        btn2 = QtGui.QPushButton('Choose calibration file', w)
        btn2.move(10,240)

        #Listbox for calibration file
        listb = QtGui.QListWidget(w)
        listb.move(10,270)
        listb.resize(580,30)

        # label for checkboxes
        checkbox_label = QtGui.QLabel('Choose processing type:',w)
        checkbox_label.move(10,305)

        # checkboxes for AC and LP processing
        cb = QtGui.QCheckBox('AC',w)
        cb.move(10,325)
        cb2 = QtGui.QCheckBox('LP',w)
        cb2.move(100,325)

        # button for exiting / running processing
        btn3 = QtGui.QPushButton('Run processing', w)
        btn3.move(10,350)

        # Create the actions for folder/file selection
        @pyqtSlot()
        def on_click_add():
            print('choosing processing folders')
            print self.config['variables']['PATHS']['topdir']
            file_dialog = QtGui.QFileDialog(w, "", self.config['variables']['PATHS']['topdir'])
            file_dialog.setFileMode(QtGui.QFileDialog.DirectoryOnly)

            # *Emma* has no idea what this does. But will keep it in regardless...
            if sys.platform == "darwin": # The native dialog doesn't allow selecting >1 directory
                file_dialog.setOption(QtGui.QFileDialog.DontUseNativeDialog)

            tree_view = file_dialog.findChild(QtGui.QTreeView)
            tree_view.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
            list_view = file_dialog.findChild(QtGui.QListView, "listView")
            list_view.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)

            if file_dialog.exec_() == QtGui.QDialog.Accepted:
                dir_list = file_dialog.selectedFiles()
                for dir in dir_list:
                    lista.addItem(str(dir))
                    #lista.addItem(os.path.basename(str(dir)))
                    w.processing_dirs.append(str(dir))

            print w.processing_dirs

            w.show()

        @pyqtSlot()
        def on_click_rm():
            print('removing processing folders')
            selected_item = lista.currentItem()
            selected_item_text = lista.currentItem().text()
            lista.removeItemWidget(selected_item)
            lista.clear()
            print(selected_item_text)
            w.processing_dirs.remove(selected_item_text)
            print("processing dirs now:")
            print(w.processing_dirs)
            for dir in w.processing_dirs:
                lista.addItem(str(dir))
                #lista.addItem(os.path.basename(str(dir)))

        @pyqtSlot()
        def on_click_cal():
            print('choosing calibration file')

            listb.clear()
            f = QtGui.QFileDialog.getOpenFileName(w, 'Open file',self.config['variables']['PATHS']['cal_pname'])
            listb.addItem(str(f))

            # make data accessible
            w.cal_file = str(f)
            print w.cal_file

            # Show window
            w.show()

        @pyqtSlot()
        def checkbox_cb(self):
            w.AC = not w.AC

        @pyqtSlot()
        def checkbox_cb2(self):
            w.LP = not w.LP

        # connect the signals to the slots
        btn_add.clicked.connect(on_click_add)
        btn_rm.clicked.connect(on_click_rm)
        btn2.clicked.connect(on_click_cal)
        btn3.clicked.connect(a.instance().quit)
        cb.clicked.connect(checkbox_cb)
        cb2.clicked.connect(checkbox_cb2)

        # execute GUI commands
        w.show()
        w.raise_()
        a.exec_()

        # set up experiment files
        # let's assume that this is already done because I'm not sure how to automate it

        # fitting
        fitting_class = fitting()
        fitting_class.fitting_loop(w.processing_dirs,w.AC,w.LP)

        print("GOT HERE!!!!!!!!!!!!!!!")
        # calibration


        return (w.AC, w.LP, w.cal_file, w.processing_dirs)
