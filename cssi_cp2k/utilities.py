import numpy as np

def is_numeric(val):
  try:
    v = float(val)
    return True
  except ValueError:
    return False
