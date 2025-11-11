import tomllib
import itertools
import math
import tabulate
import random
import numpy as np

with open("engines.toml", mode="rb") as fp:
    config = tomllib.load(fp)


def powerset(iterable):
    s = list(iterable)
    return itertools.chain.from_iterable(
        itertools.combinations(s, r) for r in range(len(s) + 1)
    )


CATASTROPHIC = -10
AMAZING = 10
FINE = 5
BAD = 0


def hand_utility(engine, non_engine):
    if engine < 1:
        return CATASTROPHIC
    elif engine >= 2 and non_engine >= 2:
        return AMAZING
    elif engine >= 1 and non_engine >= 1:
        return FINE
    else:
        return BAD


def hand_utility_vectorized(engine, non_engine):
    U = np.full(engine.shape, BAD, dtype=float)  # start with BAD
    # CATASTROPHIC
    U[engine < 1] = CATASTROPHIC

    # FINE
    U[(engine >= 1) & (non_engine >= 1)] = FINE

    # AMAZING
    U[(engine >= 2) & (non_engine >= 2)] = AMAZING

    # BAD is default, so nothing to do
    return U


def sample_hand(engine, non_engine, brick, size=5):
    totals = [0, 0, 0]
    for _ in range(size):
        card = random.randint(0, engine + non_engine + brick)
        if card < engine:
            totals[0] += 1
        elif card < engine + non_engine:
            totals[1] += 1
        else:
            totals[2] += 1
    return totals


def deck_utility(engine, non_engine, bricks, samples=10000, risk_aversion=1):
    utilities = [0 for _ in range(samples)]
    for i in range(samples):
        hand = sample_hand(engine, non_engine, bricks)
        utilities[i] = hand_utility(hand[0], hand[1])

    mean = sum(utilities) / samples
    variance = sum((u - mean) ** 2 for u in utilities) / samples
    stdv = math.sqrt(variance)

    risk_aversion = 0
    final_score = mean - risk_aversion * stdv

    return mean, variance, stdv, final_score


def deck_utility_vectorized(
    engine, non_engine, bricks, samples=30000, risk_aversion=0.25
):
    # Build deck array: 0=engine,1=non,2=brick
    deck = np.concatenate(
        [
            np.zeros(engine, dtype=np.int8),
            np.ones(non_engine, dtype=np.int8),
            np.full(bricks, 2, dtype=np.int8),
        ]
    )

    # sample without replacement
    rng = np.random.default_rng()
    hands = np.array([rng.choice(deck, size=5, replace=False) for _ in range(samples)])

    # Count engines and non-engines in each hand
    e_counts = np.sum(hands == 0, axis=1)
    n_counts = np.sum(hands == 1, axis=1)
    utilities = hand_utility_vectorized(e_counts, n_counts)

    # Compute statistics
    mean = np.mean(utilities)
    variance = np.var(utilities)
    stdv = np.sqrt(variance)
    final_score = mean - risk_aversion * stdv
    p_catastrophic = np.mean(e_counts < 1)

    return mean, variance, stdv, final_score, p_catastrophic


required = config.get("required", [])
engines = config["engines"]


def validate_build(build):
    for engine in required:
        if engine not in build:
            return False

    for engine in build:
        for requirement in engines[engine].get("requires", []):
            if requirement not in build:
                return False

    return True


results = []

for build in powerset(config["engines"].keys()):
    build = list(build)
    if not validate_build(build):
        continue

    totals = [0, 0, 0, 0]  # engine, non-engine, bricks, extra
    for engine in build:
        spaces = engines[engine].get("space", dict())
        totals[0] += spaces.get("engine", 0)
        totals[2] += spaces.get("bricks", 0)
        totals[3] += spaces.get("extra", 0)

    for non_engine_count in range(1, 30):
        totals[1] = non_engine_count
        main = sum(totals[0:3])
        extra = totals[3]
        if main < 40 or main > 60 or extra > 15:
            continue
        mean, variance, stdv, score, p_catastrophic = deck_utility_vectorized(
            totals[0], totals[1], totals[2]
        )
        results.append(
            (
                ", ".join(build),
                main,
                *totals,
                mean,
                variance,
                stdv,
                f"{round(p_catastrophic * 100)}%",
                score,
            )
        )

results.sort(key=lambda result: result[-1])

print(
    tabulate.tabulate(
        results,
        headers=(
            "Build",
            "Main",
            "EN",
            "NE",
            "BR",
            "EX",
            "Mean",
            "Variance",
            "SD",
            "Catastrophic",
            "Score",
        ),
    )
)
