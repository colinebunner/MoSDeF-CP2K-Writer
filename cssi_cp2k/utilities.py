import numpy as np

def is_number(val):
  try:
    v = float(val)
    return True
  except ValueError:
    return False
