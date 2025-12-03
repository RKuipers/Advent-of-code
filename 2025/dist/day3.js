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
import * as fs from "fs/promises"; // Use promises for asynchronous file reading
import * as path from "path";
import * as U from "./utils.js";
const dayNumber = 3;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const result = [];
        for (const line of lines) {
            const parts = line.trim().split(""); // Split by one or more spaces
            result.push(A.map((c) => parseInt(c))(parts));
        }
        return result;
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const partA = flow(A.map((batteries) => {
    const d1 = pipe(batteries, A.dropRight(1), A.reduce(0, Ord.max(N.Ord)));
    const d2 = pipe(batteries, A.dropLeftWhile((n) => n !== d1), A.dropLeft(1), A.reduce(0, Ord.max(N.Ord)));
    return 10 * d1 + d2;
}), concatAll(N.MonoidSum));
// --- Part B Logic ---
const step = (stepsAfter, accValue) => (batteries) => pipe(batteries, A.dropRight(stepsAfter), A.reduce(0, Ord.max(N.Ord)), (digit) => ({
    value: digit + accValue,
    rem: pipe(batteries, A.dropLeftWhile((n) => n !== digit), A.dropLeft(1)),
}));
const partB = flow(A.map((batteries) => pipe(NEA.range(0, 11), A.reverse, A.reduce({ value: 0, rem: batteries }, ({ value, rem }, i) => step(i, 10 * value)(rem)), ({ value }) => value)), concatAll(N.MonoidSum));
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
    }
    catch (e) {
        console.error("Execution failed.");
    }
}
main(); // Uncomment to run
// --- Just for importing ---
const uncalled = () => {
    const a = pipe([], A.map(() => ["", 0]), R.fromEntries, B.isBoolean);
    const b = flow((x) => U.modulo(x, 5));
    const c = pipe(NEA.range(1, 5), concatAll(N.MonoidSum));
};
//# sourceMappingURL=day3.js.map