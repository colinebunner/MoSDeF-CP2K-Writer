Examples
========

Short
^^^^^
This example instantiates a simulation and tries to set the RUN_TYPE variable in the GLOBAL section twice. The first time,
it succeds since MC is a valid choice. The second time, it fails as ARBITRARY is not a valid directive.

>>> from cssi_cp2k.classes import SIM as sim
>>>
>>> mySim = sim.SIM()
>>> mySim.GLOBAL.RUN_TYPE = "MC"
>>> mySim.write_changeLog()
>>> mySim.GLOBAL.RUN_TYPE = "ARBITRARY"
>>> mySim.write_errorLog()
>>> mySim.write_changeLog()

The resulting changelog is

.. code-block:: text

    SIM/GLOBAL/RUN_TYPE
      
    New: MC
    Previous: None
    Date: 2020-10-05 02:10:27.806677
    Success: True
    ErrorMessage: None

    SIM/GLOBAL/RUN_TYPE

    New: ARBITRARY
    Previous: MC
    Date: 2020-10-05 02:10:27.806776
    Success: False
    ErrorMessage: RUN_TYPE ARBITRARY not allowed. Check for typo. Allowed values are: ['BAND', 'BSSE', 'CELL_OPT', 'DEBUG', 'DRIVER', 'EHRENFEST_DYN', 'ELECTRONIC_SPECTRA', 'ENERGY', 'ENERGY_FORCE', 'GEOMETRY_OPTIMIZATION', 'GEO_OPT', 'LINEAR_RESPONSE', 'LR', 'MC', 'MOLECULAR_DYNAMICS', 'MD', 'MONTECARLO', 'NEGF', 'NONE', 'NORMAL_MODES', 'PINT', 'RT_PROPAGATION', 'SPECTRA', 'TAMC', 'TMC', 'VIBRATIONAL_ANALYSIS', 'WAVEFUNCTION_OPTIMIZATION', 'WFN_OPT']

More Realistic
^^^^^^^^^^^^^^
Coming soon