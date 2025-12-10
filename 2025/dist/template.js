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
const dayNumber = 1;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const result = [];
        for (const line of lines) {
            const parts = line.trim().split(/\s+/);
            result.push(parts.length);
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
const partA = flow((x) => 0);
const partA2 = (parsed) => pipe(parsed, (x) => 0);
// --- Part B Logic ---
const partB = flow((x) => 0);
const partB2 = (parsed) => pipe(parsed, (x) => 0);
// --- Execution ---
async function main() {
    const file = `inputs/day${dayNumber}input.txt`;
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
main(); // Uncomment to run
// --- Just for importing ---
const uncalled = () => {
    const a = pipe([], A.map(() => ["", 0]), R.fromEntries, R.toEntries, A.map(T.fst), B.isBoolean);
    const b = flow((x) => U.modulo(x, 5));
    const c = pipe(NEA.range(1, 5), concatAll(N.MonoidSum));
    const d = pipe([1, 2, 3, -1, 5, -7], A.map(flow(JSON.stringify, O.some)), A.reduce(O.none, Ord.max(O.getOrd(S.Ord))));
    const e = N.Eq;
};
//# sourceMappingURL=template.js.map