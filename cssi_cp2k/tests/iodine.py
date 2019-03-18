#quick test that works on my laptop
from cssi_cp2k.classes import SIM as sim

mySim = sim.SIM()
mySim.GLOBAL.RUN_TYPE = "MC"
mySim.MOTION.MD.PRINT.ENERGY.EACH.MD = 20
mySim.write_changeLog()
