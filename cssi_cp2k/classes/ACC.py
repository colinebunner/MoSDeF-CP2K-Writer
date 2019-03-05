import cssi_cp2k.utilities

class ACC:

  def __init__(self,AVOID_AFTER_BUSY=False,BINNING_BINSIZE=16,BINNING_NBINS=4096,MIN_FLOP_PROCESS=0,
               MIN_FLOP_SORT=4000,POSTERIOR_BUFFERS=80,POSTERIOR_STREAMS=4,PRIORITY_BUFFERS=40,
               PRIORITY_STREAMS=4,PROCESS_INHOMOGENOUS=True,STACK_SORT=True,errors=[]):

    self.__AVOID_AFTER_BUSY     = AVOID_AFTER_BUSY
    self.__BINNING_BINSIZE      = BINNING_BINSIZE
    self.__BINNING_NBINS        = BINNING_NBINS
    self.__MIN_FLOP_PROCESS     = MIN_FLOP_PROCESS
    self.__MIN_FLOP_SORT        = MIN_FLOP_SORT
    self.__POSTERIOR_BUFFERS    = POSTERIOR_BUFFERS
    self.__POSTERIOR_STREAMS    = POSTERIOR_STREAMS
    self.__PRIORITY_BUFFERS     = PRIORITY_BUFFERS
    self.__PRIORITY_STREAMS     = PRIORITY_STREAMS
    self.__PROCESS_INHOMOGENOUS = PROCESS_INHOMOGENOUS
    self.__STACK_SORT           = STACK_SORT
    self.__errors               = errors

  @property
  def AVOID_AFTER_BUSY(self):
    return self.__AVOID_AFTER_BUSY

  @property
  def BINNING_BINSIZE(self):
    return self.__BINNING_BINSIZE

  @property
  def BINNING_NBINS(self):
    return self.__BINNING_NBINS

  @property
  def MIN_FLOP_PROCESS(self):
    return self.__MIN_FLOP_PROCESS

  @property
  def MIN_FLOP_SORT(self):
    return self.__MIN_FLOP_SORT

  @property
  def POSTERIOR_BUFFERS(self):
    return self.__POSTERIOR_BUFFERS

  @property
  def POSTERIOR_STREAMS(self):
    return self.__POSTERIOR_STREAMS

  @property
  def PRIORITY_BUFFERS(self):
    return self.__PRIORITY_BUFFERS

  @property
  def PRIORITY_STREAMS(self):
    return self.__PRIORITY_STREAMS

  @property
  def PROCESS_INHOMOGENOUS(self):
    return self.__PROCESS_INHOMOGENOUS

  @property
  def STACK_SORT(self):
    return self.__STACK_SORT

  @property
  def errors(self):
    return self.__errors

  @AVOID_AFTER_BUSY.setter
  def AVOID_AFTER_BUSY(self,val):
    if isinstance(val,bool):
      self.__AVOID_AFTER_BUSY = val
    else:
      errorMessage = "Type: Setter\nVar.: AVOID_AFTER_BUSY\nErr.: AVOID_AFTER_BUSY must be a logical"
      self.__errors.append(errorMessage)

  @BINNING_BINSIZE.setter
  def BINNING_BINSIZE(self,val):
    if utilities.is_positive_integer(val):
      self.__BINNING_BINSIZE = val
    else:
      errorMessage = "Type: Setter\nVar.: BINNING_BINSIZE\nErr.: BINNING_BINSIZE must be an integer."
      self.__errors.append(errorMessage)
  
  @BINNING_NBINS.setter
  def BINNING_NBINS(self,val):
    if utilities.is_positive_integer(val):
      self.__BINNING_NBINS = val
    else:
      errorMessage = "Type: Setter\nVar.: BINNING_NBINS\nErr.: BINNING_NBINS must be an integer."
      self.__errors.append(errorMessage)

  @MIN_FLOP_PROCESS.setter
  def MIN_FLOP_PROCESS(self,val):
    if utilities.is_integer(val):
      self.__MIN_FLOP_PROCESS = val
    else:
      errorMessage = "Type: Setter\nVar.: MIN_FLOP_PROCESS\nErr.: MIN_FLOP_PROCESS must be an integer."
      self.__errors.append(errorMessage)

  @POSTERIOR_BUFFERS.setter
  def POSTERIOR_BUFFERS(self,val):
    if utilities.is_positive_integer(val):
      self.__POSTERIOR_BUFFERS = val
    else:
      errorMessage = ("Type: Setter\nVar.: POSTERIOR_BUFFERS\nErr.: POSTERIOR_BUFFERS must be an"
        " integer.")
      self.__errors.append(errorMessage)

  @PRIORITY_BUFFERS.setter
  def PRIORITY_BUFFERS(self,val):
    if utilities.is_positive_integer(val):
      self.__PRIORITY_BUFFERS = val
    else:
      errorMessage = "Type: Setter\nVar.: PRIORITY_BUFFERS\nErr.: PRIORITY_BUFFERS must be an integer."
      self.__errors.append(errorMessage)

  @PRIORITY_STREAMS.setter
  def PRIORITY_STREAMS(self,val):
    if utilities.is_positive_integer(val):
      self.__PRIORITY_STREAMS = val
    else:
      errorMessage = "Type: Setter\nVar.: PRIORITY_STREAMS\nErr.: PRIORITY_STREAMS must be an integer."
      self.__errors.append(errorMessage)

  @PROCESS_INHOMOGENOUS.setter
  def PROCESS_INHOMOGENOUS(self,val):
    if isinstance(val,bool):
      self.__PROCESS_INHOMOGENOUS = val
    else:
      errorMessage = ("Type: Setter\nVar.: PROCESS_INHOMOGENOUS\nErr.: PROCESS_INHOMOGENOUS must be a "
        "boolean.")
      self.__errors.append(errorMessage)

  @STACK_SORT.setter
  def STACK_SORT(self,val):
    if isinstance(val,bool):
      self.__STACK_SORT = val
    else:
      errorMessage = "Type: Setter\nVar.: STACK_SORT\nErr.: STACK_SORT must be a boolean."
      self.__errors.append(errorMessage)
