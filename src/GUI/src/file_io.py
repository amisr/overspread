import os
import sys
import glob

class FileIO:

    def __init__(self, config, logger = None):
        self.logger = logger
        self.config = config

    """
    def readafile(fname):
        h5file=tables.openFile(fname)
        output={}
        for group in h5file.walkGroups("/"):
            output[group._v_pathname]={}
            for array in h5file.listNodes(group, classname = 'Array'):
                output[group._v_pathname][array.name]=array.read()
        h5file.close()

        return output
    """

    def construct_full_path(self, kwargs):
        exp_dir = kwargs["exp_dir"]
        exp_type = kwargs["exp_type"]
        exp_name = kwargs["exp_name"]

        cal_file = kwargs["cal_file"]

        exp_path = os.path.join(self.config["variables"]["PATHS"]["topdir"],
                                exp_dir,
                                exp_type,
                                exp_name)
        cal_path = os.path.join(self.config["variables"]["PATHS"]["cal_pname"],
                                cal_file.split('/')[1][0:6],
                                cal_file)

        if not os.path.isdir(os.path.dirname(exp_path)):
            self.logger.error("Cannot find experiment path!")
            sys.exit(0)

        if not os.path.exists(cal_path):
            self.logger.error("Cannot find calibration file!")
            sys.exit(0)

        # Generating list of folders we want to process in the experiment folders
        experiment_list = glob.glob(exp_path)


        # Iterating through list to eliminate aliased directories
        short_experiment_list = []
        for experiment in experiment_list:
            if not os.path.islink(experiment):
                short_experiment_list.append(experiment)
