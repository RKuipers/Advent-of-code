import * as A from "fp-ts/lib/Array.js";
import * as E from "fp-ts/lib/Either.js";
import { flow, pipe } from "fp-ts/lib/function.js";
import * as N from "fp-ts/lib/number.js";
import * as O from "fp-ts/lib/Option.js";
import * as R from "fp-ts/lib/Record.js";
import * as fs from "fs/promises"; // Use promises for asynchronous file reading
import * as path from "path";

type ParseType = { llist: number[]; rlist: number[] };

// --- Data Preparation Helper ---

async function readAndParseData(filePath: string): Promise<ParseType> {
  try {
    const data = await fs.readFile(path.resolve(filePath), "utf8");
    const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines

    // UPDATE FROM HERE

    const llist: number[] = [];
    const rlist: number[] = [];

    for (const line of lines) {
      const parts = line.trim().split(/\s+/); // Split by one or more spaces
      if (parts.length === 2) {
        llist.push(parseInt(parts[0]!, 10));
        rlist.push(parseInt(parts[1]!, 10));
      }
    }
    return { llist, rlist };
  } catch (error) {
    console.error("Error reading file:", error);
    throw error;
  }
}

// --- Part A Logic ---

const d1a = (parsed: ParseType): number => {
  // UPDATE FROM HERE
  // USE parsed.x if parsed type is an object

  const lsorted = A.sort(N.Ord)(parsed.llist);
  const rsorted = A.sort(N.Ord)(parsed.rlist);

  return pipe(
    A.zip(lsorted)(rsorted),
    A.foldMap(N.MonoidSum)(([a, b]) => Math.abs(a - b))
  );
};

// --- Part B Logic ---

const d1b = (parsed: ParseType): number => {
  // UPDATE FROM HERE
  // USE parsed.x if parsed type is an object

  const count = (l: Array<number>) =>
    A.reduce({} as Record<string, number>, (r, n: number) => {
      const v = R.lookup(n.toString(), r);
      return R.upsertAt(n.toString(), O.isSome(v) ? v.value + 1 : 1)(r);
    })(l);

  const lcounts = count(parsed.llist);
  const rcounts = count(parsed.rlist);

  const products = R.intersection(N.MonoidProduct)(rcounts)(lcounts);

  return pipe(
    products,
    R.toArray,
    A.foldMap(N.MonoidSum)(([k, v]) => Number(k) * v)
  );
};

// --- Execution ---

async function main() {
  const file = "inputs/day1input_2024.txt";

  try {
    const startParse = performance.now();
    const parsed = await readAndParseData(file);

    const startA = performance.now();
    const resultA = d1a(parsed);

    const startB = performance.now();
    const resultB = d1b(parsed);
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
