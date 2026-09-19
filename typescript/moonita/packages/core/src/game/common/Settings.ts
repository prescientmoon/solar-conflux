const mul = 1;

export const settings = {
  maxBoidForce: 0.05,
  maxBoidVelocity: 4,
  maxBoids: 500,
  alignmentRadius: mul * 50,
  alignmentCoefficient: mul,
  separationRadius: mul * 30,
  separationDifferentTeamRadius: mul * 100,
  separationCoefficient: mul * 1.5,
  separationDifferentTeamDistanecMultiplier: 10,
  separationDiffereTeamCoefficient: 5,
  cohesionRadius: mul * 50,
  cohesionCoefficient: mul,
  seekingCoefficinet: mul,
  pathFollowingCoefficient: mul / 1.2,
  maxDeckSize: 30,
  ups: 30,
};

export const maxBoidRadius = Math.max(
  settings.alignmentRadius,
  settings.separationRadius,
  settings.cohesionRadius
);
