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
const dayNumber = 5;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const ranges = [];
        const entries = [];
        for (const line of lines) {
            if (line.includes("-")) {
                const parts = line.trim().split("-");
                ranges.push([parseInt(parts[0]), parseInt(parts[1])]);
            }
            else if (line.trim().length > 0)
                entries.push(parseInt(line.trim()));
        }
        return { ranges, entries };
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const inRange = (x) => (range) => x >= range[0] && x <= range[1];
const partA = (parsed) => pipe(parsed.entries, A.map((entry) => pipe(parsed.ranges, A.findFirst(inRange(entry)), O.match(() => 0, () => 1))), concatAll(N.MonoidSum));
// --- Part B Logic ---
const rangeOrd = Ord.contramap(T.fst)(N.Ord);
const mergeRanges = (range1) => (range2) => range1[1] < range2[0] || range1[0] > range2[1]
    ? [range1, range2]
    : [[Math.min(range1[0], range2[0]), Math.max(range1[1], range2[1])]];
const partB = (parsed) => pipe(parsed.ranges, A.sort(rangeOrd), A.reduce([], (acc, next) => acc.length === 0
    ? [next]
    : [
        ...A.dropRight(1)(acc),
        ...pipe(acc, A.last, O.match(() => [next], (last) => mergeRanges(last)(next))),
    ]), A.map(([min, max]) => max - min + 1), concatAll(N.MonoidSum));
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
    const d = pipe([1, 2, 3, -1, 5, -7], A.map(flow(JSON.stringify, O.some)), A.reduce(O.none, Ord.max(O.getOrd(S.Ord))));
};
//# sourceMappingURL=day5.js.map