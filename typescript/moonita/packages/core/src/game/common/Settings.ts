const mul = 1;

export const settings = {
  maxBoidForce: 0.05,
  maxBoidVelocity: 3,
  maxBoids: 1000,
  alignmentRadius: 50,
  alignmentCoefficient: mul,
  separationRadius: 25,
  separationCoefficient: mul * 1.5,
  cohesionRadius: 50,
  cohesionCoefficient: mul,
  seekingCoefficinet: mul,
  pathFollowingCoefficient: mul / 1.2,
};

export const maxBoidRadius = Math.max(
  settings.alignmentRadius,
  settings.separationRadius,
  settings.cohesionRadius
);
