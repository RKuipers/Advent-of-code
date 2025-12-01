import * as A from "fp-ts/lib/Array.js";
import * as E from "fp-ts/lib/Either.js";
import { flow, pipe } from "fp-ts/lib/function.js";
import * as O from "fp-ts/lib/Option.js";
import * as R from "fp-ts/lib/Record.js";
import * as fs from "fs/promises"; // Use promises for asynchronous file reading
import * as path from "path";

type ParseType = Array<number>;
const dayNumber = 1;

// --- Data Preparation Helper ---

async function readAndParseData(filePath: string): Promise<ParseType> {
  try {
    const data = await fs.readFile(path.resolve(filePath), "utf8");
    const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines

    // UPDATE FROM HERE

    const list: number[] = [];

    for (const line of lines) {
      const l = line.trim();
      if (l[0] === "R") list.push(parseInt(l.slice(1)));
      else list.push(-1 * parseInt(l.slice(1)));
    }
    return list;
  } catch (error) {
    console.error("Error reading file:", error);
    throw error;
  }
}

// --- Part A Logic ---

const partA = flow(
  A.reduce({ count: 0, current: 50 }, (acc, v: number) => {
    const next = (acc.current + v) % 100;
    return { count: next === 0 ? acc.count + 1 : acc.count, current: next };
  }),
  ({ count }) => count
);

// --- Part B Logic ---

const partB = flow(
  A.reduce({ count: 0, current: 50 }, (acc, v: number) => {
    const fullTurns = Math.floor(Math.abs(v) / 100);
    const rem = v % 100;
    const next = acc.current + rem;
    const newCount =
      fullTurns + ((next <= 0 && acc.current !== 0) || next >= 100 ? 1 : 0);

    return {
      count: acc.count + newCount,
      current: next < 0 ? 100 + next : next % 100,
    };
  }),
  ({ count }) => count
);

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
  const x = pipe(
    [],
    A.map(() => ["", 0] as const satisfies [string, number]),
    R.fromEntries
  );
  const y = flow;
};
