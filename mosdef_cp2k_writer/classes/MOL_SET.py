import datetime
import mosdef_cp2k_writer.utilities as utilities
from mosdef_cp2k_writer.utilities1 import oneDimArray as oda
from mosdef_cp2k_writer.utilities1 import objectArray as oba

from mosdef_cp2k_writer.classes import MOLECULEs


class MOL_SET:
    def __init__(self, errorLog=[], changeLog=[], location=""):

        self.__errorLog = errorLog
        self.__changeLog = changeLog

        self.__location = "{}/MOL_SET".format(location)
        # subsections
        #self.__MOLECULE = MOLECULE.MOLECULE(
        #    errorLog=self.__errorLog,
        #    changeLog=self.__changeLog,
        #    location=self.__location,
        #)

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
    def MOLECULE(self):
        return self.__MOLECULE
    
    def init_molecules(self, nmoleculetype):
        MOLECULE = []
        for i in range(nmoleculetype):
            MOLECULE.append(
                MOLECULEs.MOLECULEs(
                    number=i + 1,
                    errorLog=self.__errorLog,
                    changeLog=self.__changeLog,
                    location=self.__location,
                )
            )
        self.__MOLECULE = oba.objectArray.listToOBA(
            MOLECULE,
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )
