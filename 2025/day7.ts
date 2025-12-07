import * as A from "fp-ts/lib/Array.js";
import * as B from "fp-ts/lib/boolean.js";
import * as E from "fp-ts/lib/Either.js";
import { flow, pipe } from "fp-ts/lib/function.js";
import { concatAll } from "fp-ts/lib/Monoid.js";
import * as NEA from "fp-ts/lib/NonEmptyArray.js";
import * as N from "fp-ts/lib/number.js";
import * as O from "fp-ts/lib/Option.js";
import * as Ord from "fp-ts/lib/Ord.js";
import * as R from "fp-ts/lib/Record.js";
import * as S from "fp-ts/lib/string.js";
import * as T from "fp-ts/lib/Tuple.js";
import * as fs from "fs/promises";
import * as path from "path";
import * as U from "./utils.js";

// npx tsc && node dist/day1.js

type ParseType = { splits: Array<Array<number>>; start: number };
type PartType = (parsed: ParseType) => number;
const dayNumber = 7;

// --- Data Preparation Helper ---

async function readAndParseData(filePath: string): Promise<ParseType> {
  try {
    const data = await fs.readFile(path.resolve(filePath), "utf8");
    const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines

    // UPDATE FROM HERE

    const start = pipe(
      lines,
      A.head,
      O.flatMap((line) =>
        pipe(
          line.trim().split(""),
          A.findIndex((c) => c === "S")
        )
      ),
      O.match(
        () => 0,
        (x) => x
      )
    );

    const splits: Array<Array<number>> = pipe(
      lines,
      A.dropLeft(1),
      A.map((line) =>
        pipe(
          line.trim().split(""),
          A.reduceWithIndex([] as Array<number>, (y, acc, next) =>
            next === "^" ? [...acc, y] : acc
          )
        )
      )
    );
    return { splits, start };

    // UPDATE UNTIL HERE
  } catch (error) {
    console.error("Error reading file:", error);
    throw error;
  }
}

// --- Part A Logic ---

const intersection =
  (a: Array<number>) =>
  (b: Array<number>): Array<number> =>
    A.getIntersectionSemigroup(N.Eq).concat(a, b);
const union =
  (a: Array<number>) =>
  (b: Array<number>): Array<number> =>
    A.getUnionSemigroup(N.Eq).concat(a, b);
const difference = A.difference(N.Eq);

const step =
  (splits: Array<number>) =>
  (beams: Array<number>): { newBeams: Array<number>; newSplits: number } =>
    pipe(beams, intersection(splits), (splitBeams) =>
      pipe(
        splitBeams,
        A.flatMap((x) => [x - 1, x + 1]),
        (newBeams) =>
          pipe(
            beams,
            difference(splitBeams),
            union(newBeams),
            A.uniq(N.Eq),
            (b) => ({
              newBeams: b,
              newSplits: splitBeams.length,
            })
          )
      )
    );

const partA = (parsed: ParseType): number =>
  pipe(
    NEA.range(0, parsed.splits.length),
    A.reduce({ splitCount: 0, beams: [parsed.start] }, (acc, row) =>
      pipe(acc.beams, step(parsed.splits[row] ?? []), (result) => ({
        splitCount: acc.splitCount + result.newSplits,
        beams: result.newBeams,
      }))
    ),
    (result) => result.splitCount
  );

// --- Part B Logic ---

const partB: PartType = flow((x) => 0);
const partB2 = (parsed: ParseType): number => pipe(parsed, (x) => 0);

// --- Execution ---

async function main() {
  const file = `inputs/day${dayNumber}input.txt`;

  try {
    const startParse = performance.now();
    const parsed = await readAndParseData(file);

    const startA = performance.now();
    const resultA = partA(parsed);

    const startB = performance.now();
    const resultB = partB(parsed);
    const end = performance.now();

    console.log(resultA);
    console.log(resultB);
    console.log(`Parsing took ${(startA - startParse).toFixed(3)}ms`);
    console.log(`Part A took ${(startB - startA).toFixed(3)}ms`);
    console.log(`Part B took ${(end - startB).toFixed(3)}ms`);
  } catch (e) {
    console.error("Execution failed.");
  }
}

main(); // Uncomment to run

// --- Just for importing ---
const uncalled = () => {
  type opt<A> = O.Option<A>;
  type eth<E, A> = E.Either<E, A>;
  const a = pipe(
    [],
    A.map(() => ["", 0] as const satisfies [string, number]),
    R.fromEntries,
    R.toEntries,
    A.map(T.fst),
    B.isBoolean
  );
  const b = flow((x: number) => U.modulo(x, 5));
  const c = pipe(NEA.range(1, 5), concatAll(N.MonoidSum));
  const d = pipe(
    [1, 2, 3, -1, 5, -7],
    A.map(flow(JSON.stringify, O.some)),
    A.reduce(O.none, Ord.max(O.getOrd(S.Ord)))
  );
};
