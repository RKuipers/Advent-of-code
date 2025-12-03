import * as A from "fp-ts/lib/Array.js";
import * as B from "fp-ts/lib/boolean.js";
import * as E from "fp-ts/lib/Either.js";
import { flow, pipe } from "fp-ts/lib/function.js";
import { concatAll } from "fp-ts/lib/Monoid.js";
import * as NEA from "fp-ts/lib/NonEmptyArray.js";
import * as N from "fp-ts/lib/number.js";
import * as O from "fp-ts/lib/Option.js";
import * as R from "fp-ts/lib/Record.js";
import * as fs from "fs/promises"; // Use promises for asynchronous file reading
import * as path from "path";
import * as U from "./utils.js";
const dayNumber = 2;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        // const result: ParseType = [];
        // for (const line of lines) {
        //   const parts = line.trim().split(',');
        //   result.push(pipe(parts, A.map((r) => R.split('-'))))
        // }
        return pipe(lines, A.flatMap((a) => a.trim().split(",")), A.map((r) => {
            const s = r.split("-");
            if (s.length !== 2)
                throw "PARSING ERROR";
            return { start: parseInt(s[0]), end: parseInt(s[1]) };
        }));
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const countDigits = (x) => Math.floor(Math.log10(x)) + 1;
const partA = flow(A.flatMap((range) => {
    const startD = countDigits(range.start);
    const endD = countDigits(range.end);
    if (startD === endD && startD % 2 === 0)
        return [range];
    if (startD === endD && startD % 2 !== 0)
        return [];
    if (startD + 1 === endD && startD % 2 === 0)
        return [{ start: range.start, end: Math.pow(10, startD) - 1 }];
    if (startD + 1 === endD && endD % 2 === 0)
        return [{ start: Math.pow(10, endD - 1), end: range.end }];
    console.log(`[ERROR] Not supposed to be here`);
    return [];
}), A.flatMap((range) => {
    const digits = countDigits(range.start);
    const startHalf = Math.floor(range.start / Math.pow(10, digits / 2)); // 12345678 / 10^4 = 1234.5678
    const endHalf = Math.floor(range.end / Math.pow(10, digits / 2));
    const result = [];
    for (let i = startHalf; i <= endHalf; i++) {
        const candidate = i * Math.pow(10, digits / 2) + i;
        if (candidate >= range.start && candidate <= range.end)
            result.push(candidate);
    }
    return result;
}), concatAll(N.MonoidSum));
// --- Part B Logic ---
const divisors = (num) => pipe(NEA.range(1, num - 1), // [1, 2, ..., num-1]
A.filter((i) => num % i === 0) // keep only divisors
);
const partB = flow(A.flatMap((range) => {
    const result = [];
    for (let i = range.start; i <= range.end; i++)
        result.push(i);
    return result;
}), A.filter((x) => {
    if (x < 10)
        return false;
    const digits = countDigits(x);
    const divs = divisors(digits);
    return pipe(divs, A.map((digitCount) => {
        const firstDigits = Math.floor(x / Math.pow(10, digits - digitCount));
        const recon = A.reduce(firstDigits, (acc, a) => acc * Math.pow(10, digitCount) + firstDigits)(NEA.range(1, digits / digitCount - 1));
        return x === recon;
    }), concatAll(B.MonoidAny));
}), concatAll(N.MonoidSum));
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
    const x = pipe([], A.map(() => ["", 0]), R.fromEntries);
    const y = flow((x) => U.modulo(x, 5));
};
//# sourceMappingURL=day2.js.map