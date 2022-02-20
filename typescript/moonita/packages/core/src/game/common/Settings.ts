const mul = 1;

export const settings = {
  maxBoidForce: 0.05,
  maxBoidVelocity: 4,
  maxBoids: 1000,
  alignmentRadius: 50,
  alignmentCoefficient: mul,
  separationRadius: 30,
  separationDifferentTeamRadius: 50,
  separationCoefficient: mul * 1.5,
  separationDiffereTeamCoefficient: 10,
  cohesionRadius: 50,
  cohesionCoefficient: mul,
  seekingCoefficinet: mul,
  pathFollowingCoefficient: mul / 1.2,
  ups: 30,
};

export const maxBoidRadius = Math.max(
  settings.alignmentRadius,
  settings.separationRadius,
  settings.cohesionRadius
);
