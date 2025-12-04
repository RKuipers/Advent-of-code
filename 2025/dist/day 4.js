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
import * as fs from "fs/promises"; // Use promises for asynchronous file reading
import * as path from "path";
import * as U from "./utils.js";
const dayNumber = 4;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const result = pipe(lines, A.mapWithIndex((y, line) => pipe(line.trim().split(""), A.mapWithIndex((x, char) => [`${x},${y}`, char === "@"]), R.fromEntries)), concatAll(R.getUnionMonoid(B.MonoidAny)));
        return result;
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const lookup = (coords) => ([x, y]) => coords[`${x},${y}`] ?? false;
const isCoord = (arr) => arr.length === 2;
const offsets = pipe(NEA.range(-1, 1), A.flatMap((dx) => pipe(NEA.range(-1, 1), A.filterMap((dy) => dx !== 0 || dy !== 0
    ? O.some([dx, dy])
    : O.none))));
const neighbors = (coord) => pipe(coord.split(","), A.map(parseInt), (coord) => !isCoord(coord)
    ? []
    : pipe(offsets, A.map(([dx, dy]) => [coord[0] + dx, coord[1] + dy])));
const partA = (coords) => pipe(coords, R.mapWithIndex((coord, v) => v
    ? pipe(coord, neighbors, A.map(flow(lookup(coords), (b) => (b ? 1 : 0))), concatAll(N.MonoidSum), O.some)
    : O.none), R.collect(S.Ord)((_, n) => (O.isSome(n) && n.value <= 4 ? 1 : 0)), concatAll(N.MonoidSum));
// --- Part B Logic ---
const partB = flow(() => 0);
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
    const d = pipe([1, 2, 3, -1, 5, -7], A.map(O.some), A.reduce(O.none, Ord.max(O.getOrd(N.Ord))));
};
//# sourceMappingURL=day%204.js.map