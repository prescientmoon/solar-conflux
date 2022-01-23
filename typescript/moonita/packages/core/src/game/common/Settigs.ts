const mul = 1;

export const settings = {
  maxBoidForce: 0.05,
  maxBoidVelocity: 3,
  alignmentRadius: 50 ** 2,
  alignmentCoefficient: mul,
  separationRadius: 25 ** 2,
  separationCoefficient: mul * 1.5,
  cohesionRadius: 50 ** 2,
  cohesionCoefficient: mul,
};
