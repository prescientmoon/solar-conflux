import numpy as np

def decompose(M):
  n = len(M)

  U = M.copy()
  L = np.identity(n)

  for i in range(0, n - 1):
    for j in range(i + 1, n):
      L[j, i] = U[j, i] / U[i, i]
      U[j, i:n] -= L[j, i] * U[i, i:n]

  assert np.allclose(M, L@U)

  return L, U

def solve_lower(m, v):
  x = np.zeros(len(v))

  for i in range(0, len(v)):
    x[i] = (v[i] - m[i, :i] @ x[:i]) / m[i, i]

  assert np.allclose(m@x, v)

  return x

def solve_upper(m, v):
  x = np.zeros(len(v))

  for i in reversed(range(0, len(m))):
    x[i] = (v[i] - m[i, i+1:] @ x[i+1:]) / m[i, i]

  assert np.allclose(m@x, v)

  return x

def solve(m, v):
  L, U = decompose(m)
  x = solve_upper(U, solve_lower(L, v))

  assert np.allclose(m@x, v)

  return x
