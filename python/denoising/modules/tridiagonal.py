import numpy as np
import scipy as sp

def assert_valid_tridiagonal(a, c, e):
  """
  Validates a tridiagonal matrix.
  """
  assert len(a) == len(c) + 1 == len(e) + 1

def create(a, c, e):
  """
  Validates a tridiagonal matrix before creating it.
  """
  assert_valid_tridiagonal(a, c, e)
  return (a, c, e)


def decompose(a, c, e):
  """
  Computes the LU decomposition of a tridiagonal matrix.
  """
  assert_valid_tridiagonal(a, c, e)

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
  assert_valid_tridiagonal(a, c, e)

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
  assert_valid_tridiagonal(a, c, e)
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
  return create(np.ones(len(α) + 1), np.zeros(len(α)), α) 

def from_upper(β, c):
  """
  Turns the upper vectors of a decomposition into a tridiagonal matrix.

  Example ussage:
  ```py 
  α, β = decompose((a, c, e))
  print(from_upper(β, c))
  ```
  """
  return create(β, c, np.zeros(len(c)))

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
  """
  Solves a system of linear equations defined by a tridiagonal matrix.
  """
  assert_valid_tridiagonal(a, c, e)

  α, β = decompose(a, c, e)
  x = solve_upper(β, c, solve_lower(α, rhs))

  if len(α) <= 10:
    assert np.allclose(to_array(a, c, e)@x, rhs)

  return x

def multiply_vector(a, c, e, x):
  """
  Performs a matrix-vector multiplication where the matrix is tridiagonal.
  """
  assert_valid_tridiagonal(a, c, e)
  assert len(x) == len(a)

  result = np.zeros_like(x)
  for i in range(len(x)):
    result[i] = a[i] * x[i]
    if i > 0:
      result[i] += e[i - 1] * x[i - 1]
    if i < len(x) - 1:
      result[i] += c[i] * x[i + 1]

  # Sanity check
  if len(a) <= 10:
    assert np.allclose(to_array(a, c, e) @ x, result)

  return result

def largest_eigenvalue(a, c, e, initial_x, kmax):
  """
  Computes the largest eigenvalue of a positive definite tridiagonal matrix.
  """
  assert_valid_tridiagonal(a, c, e)

  x = initial_x
  for i in range(kmax):
    q = multiply_vector(a, c, e, x)
    assert not np.allclose(q, 0)
    x = q/np.linalg.norm(q)

  # Computes the eigenvalue from the eigenvector
  eigenvalue = (x @ multiply_vector(a, c, e, x)) / (x @ x)

  # Sanity check
  if len(initial_x) <= 10:
    actual_eigenvalues, _ = np.linalg.eig(to_array(a, c, e))
    assert np.allclose(
      0,
      np.min(
        np.abs(
          actual_eigenvalues - eigenvalue
        )
      )
    )

  return eigenvalue

def add(a, b):
  """
  Adds two tridiagonal matrices.
  """
  assert_valid_tridiagonal(*a)
  assert_valid_tridiagonal(*b)

  assert len(a[0]) == len(b[0])

  result = create(a[0] + b[0], a[1] + b[1], a[2] + b[2])

  # Sanity check
  if len(a[0]) <= 10:
    assert np.allclose(to_array(*result), to_array(*a) + to_array(*b))

  return result

def identity(n):
  """
  Returns the tridiagonal identity n*n matrix.
  """
  assert n > 0 # Sanity check
  return create(np.ones(n), np.zeros(n - 1), np.zeros(n - 1))

def scale(s, m):
  """
  Multiplies a tridiagonal matrix by a scalar
  """
  assert_valid_tridiagonal(*m)

  result = create(s * m[0], s * m[1], s * m[2])

  # Sanity check
  if len(m[0]) <= 10:
    assert np.allclose(result, s * to_array(*m))

  return result

# Small sanity check for the above code
def main():
  a, c, e = create(3*np.ones(4), 2*np.ones(3), 3*np.ones(3))

  rhs = np.ones(4)
  result = solve(a, c, e, rhs)
  print(f"m={to_array(a, c, e)}")
  print(f"{rhs=}")
  print(f"{result=}")
  print(to_array(a, c, e) @ result)
  print(largest_eigenvalue(a, c, e, np.ones(4), 50))

# main()
