import datetime
from mosdef_cp2k_writer.utilities import test_instance as ti
from mosdef_cp2k_writer.classes import E_DENSITY_CUBE


BOOL_VALS = [".TRUE.", ".FALSE"]


class PRINT:
    def __init__(self, errorLog=[], changeLog=[], location=""):

        self.__errorLog = errorLog
        self.__changeLog = changeLog

        self.__location = "{}/PRINT".format(location)
        # THERMOSTAT subsections
        self.__E_DENSITY_CUBE = E_DENSITY_CUBE.E_DENSITY_CUBE(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )

    @property
    def E_DENSITY_CUBE(self):
        return self.__E_DENSITY_CUBE
