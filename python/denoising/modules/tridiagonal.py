import numpy as np
import scipy as sp

def decompose(a, c, e):
  """
  Computes the LU decomposition of a tridiagonal matrix.
  """
  α = np.zeros(len(c))
  β = a.copy()

  for i in range(len(c)):
    α[i] = e[i] / β[i]
    β[i + 1] -= c[i] * α[i]

  # Sanity check
  if len(a) <= 10:
    assert np.allclose(
      to_array(a, c, e),
      to_array(*from_lower(α)) @ to_array(*from_upper(β, c))
    )

  return (α, β)

def to_csr(a, c, e):
  """
  Converts a tridiagonal matrix into a scipy csr sparse matrix.
  """
  n = len(c)

  values = np.zeros(n * 3 + 1)
  values[::3] = a
  values[1::3] = c
  values[2::3] = e
  
  col_indices = np.zeros_like(values)
  col_indices[1::3] = np.arange(1, n + 1)
  col_indices[2::3] = np.arange(0, n)
  col_indices[3::3] = np.arange(1, n + 1)

  index_ptr = np.zeros(n + 2)
  index_ptr[1:n+1] = np.arange(2, n * 3 + 2, 3)
  index_ptr[n+1] = n * 3 + 1

  return sp.sparse.csr_array((values, col_indices, index_ptr))

def to_array(a, c, e):
  """
  Converts a tridiagonal matrix into a numpy matrix.
  """
  return to_csr(a, c, e).toarray()

def from_lower(α):
  """
  Turns the lower vector of a decomposition into a tridiagonal matrix.

  Example ussage:
  ```py 
  α, β = decompose(m)
  print(from_lower(α))
  ```
  """
  return (np.ones(len(α) + 1), np.zeros(len(α)), α) 

def from_upper(β, c):
  """
  Turns the upper vectors of a decomposition into a tridiagonal matrix.

  Example ussage:
  ```py 
  α, β = decompose((a, c, e))
  print(from_upper(β, c))
  ```
  """
  return (β, c, np.zeros(len(c)))

def solve_lower(α, rhs):
  """
  Solve a linear system of equations Mx = v
  where M is a lower triangular matrix constructed
  by LU decomposing a tridiagonal matrix.
  """
  x = np.zeros_like(rhs)

  x[0] = rhs[0]

  for i in range(1, len(rhs)):
    x[i] = rhs[i] - α[i - 1] * x[i - 1]

  if len(α) <= 10:
    assert np.allclose(to_array(*from_lower(α)) @ x, rhs)

  return x

def solve_upper(β, c, rhs):
  """
  Solve a linear system of equations Mx = v
  where M is an upper triangular matrix constructed
  by LU decomposing a tridiagonal matrix.
  """
  x = np.zeros_like(rhs)

  x[-1] = rhs[-1] / β[-1]

  for i in reversed(range(len(rhs) - 1)):
    x[i] = (rhs[i] - c[i] * x[i+1]) / β[i]

  if len(β) <= 10:
    assert np.allclose(to_array(*from_upper(β, c)) @ x, rhs)

  return x

def solve(a, c, e, rhs):
  α, β = decompose(a, c, e)
  x = solve_upper(β, c, solve_lower(α, rhs))

  if len(α) <= 10:
    assert np.allclose(to_array(a, c, e)@x, rhs)

  return x

# Small sanity check for the above code
def main():
  a, c, e = (np.ones(4), 2*np.ones(3), 3*np.ones(3))

  rhs = np.ones(4)
  result = solve(a, c, e, rhs)
  print(f"m={to_array(a, c, e)}")
  print(f"{rhs=}")
  print(f"{result=}")
  print(to_array(a, c, e) @ result)

main()
