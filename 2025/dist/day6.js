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
const dayNumber = 6;
const transpose = (list) => pipe(list, A.reduce(A.replicate((list[0] ?? []).length, []), (acc, next) => pipe(A.zip(acc, next), A.map(([a, n]) => [...a, n]))));
// --- Data Preparation Helper ---
async function readAndParseDataA(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const symbols = pipe(lines, A.last, O.match(() => [], (line) => line.trim().split(/\s+/)), A.map((char) => (char === "*" ? "*" : "+")));
        const result = pipe(lines, A.dropRight(1), A.map((line) => line.trim().split(/\s+/)), transpose, A.map(A.map((entry) => parseInt(entry.trim()))), A.zip(symbols));
        return result;
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
async function readAndParseDataB(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const symbols = pipe(lines, A.last, O.match(() => [], (line) => line.trim().split(/\s+/)), A.map((char) => (char === "*" ? "*" : "+")), A.reverse);
        const result = pipe(lines, A.dropRight(1), A.map((line) => line.split("")), transpose, A.reverse, A.map((col) => parseInt(col.join(""))), A.reduce([], (acc, next) => isNaN(next)
            ? [...acc, []]
            : [
                ...A.dropRight(1)(acc),
                pipe(acc, A.last, O.match(() => [next], (last) => [...last, next])),
            ]), A.zip(symbols));
        return result;
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const partAB = flow(A.map(([numbers, symbol]) => pipe(numbers, concatAll(symbol === "+" ? N.MonoidSum : N.MonoidProduct))), concatAll(N.MonoidSum));
// --- Execution ---
async function main() {
    const file = `inputs/day${dayNumber}input.txt`;
    try {
        const startA = performance.now();
        const parsedA = await readAndParseDataA(file);
        const resultA = partAB(parsedA);
        const startB = performance.now();
        const parsedB = await readAndParseDataB(file);
        const resultB = partAB(parsedB);
        const end = performance.now();
        console.log(resultA);
        console.log(resultB);
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
    const a = pipe([], A.map(() => ["", 0]), R.fromEntries, R.toEntries, A.map(T.fst), B.isBoolean);
    const b = flow((x) => U.modulo(x, 5));
    const c = pipe(NEA.range(1, 5), concatAll(N.MonoidSum));
    const d = pipe([1, 2, 3, -1, 5, -7], A.map(flow(JSON.stringify, O.some)), A.reduce(O.none, Ord.max(O.getOrd(S.Ord))));
};
//# sourceMappingURL=day6.js.map