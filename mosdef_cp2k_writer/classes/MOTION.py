import datetime
from mosdef_cp2k_writer.utilities import test_instance as ti
from mosdef_cp2k_writer.classes import PRINT
from mosdef_cp2k_writer.classes import MD
from mosdef_cp2k_writer.classes import MC
from mosdef_cp2k_writer.classes import GEO_OPT
from mosdef_cp2k_writer.classes import CONSTRAINT


class MOTION:
    def __init__(self, errorLog=[], changeLog=[], location=""):

        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/MOTION".format(location)
        # Subsections of MOTION
        self.__PRINT = PRINT.PRINT(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )
        self.__MD = MD.MD(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )
        self.__MC = MC.MC(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )
        self.__GEO_OPT = GEO_OPT.GEO_OPT(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )
        self.__CONSTRAINT = CONSTRAINT.CONSTRAINT(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )

    @property
    def errorLog(self):
        return self.__errorLog

    @property
    def changeLog(self):
        return self.__changeLog

    @property
    def location(self):
        return self.__location

    @property
    def PRINT(self):
        return self.__PRINT

    @property
    def MD(self):
        return self.__MD

    @property
    def MC(self):
        return self.__MC

    @property
    def GEO_OPT(self):
        return self.__GEO_OPT

    @property
    def CONSTRAINT(self):
        return self.__CONSTRAINT
