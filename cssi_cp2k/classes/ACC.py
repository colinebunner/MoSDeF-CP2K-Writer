import cssi_cp2k.utilities

class ACC:

  def __init__(self,AVOID_AFTER_BUSY=False,BINNING_BINSIZE=16,BINNING_NBINS=4096,MIN_FLOP_PROCESS=0,
               MIN_FLOP_SORT=4000,POSTERIOR_BUFFERS=80,POSTERIOR_STREAMS=4,PRIORITY_BUFFERS=40,
               PRIORITY_STREAMS=4,PROCESS_INHOMOGENOUS=True,STACK_SORT=True):

    self.__AVOID_AFTER_BUSY     = AVOID_AFTER_BUSY
    self.__BINNING_BINSIZE      = BINNING_BINSIZE
    self.__BINNING_NBINS        = BINNING_NBINS
    self.__MIN_FLOP_PROCESS     = MIN_FLOP_PROCESS
    self.__MIN_FLOP_SORT        = MIN_FLOP_SORT
    self.__POSTERIOR_BUFFERS    = POSTERIOR_BUFFERS
    self.__POSTERIOR_STREAMS    = POSTER_STREAMS
    self.__PRIORITY_BUFFERS     = PRIORITY_BUFFERS
    self.__PRIORITY_STREAMS     = PRIORITY_STREAMS
    self.__PROCESS_INHOMOGENOUS = PROCESS_INHOMOGENOUS
    self.__STACK_SORT           = STACK_SORT

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
