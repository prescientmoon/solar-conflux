exports.catchErrorsImpl = (left) => (right) => (f) => (i) => {
  try {
    return right(f(i));
  } catch (e) {
    return left(e);
  }
};
