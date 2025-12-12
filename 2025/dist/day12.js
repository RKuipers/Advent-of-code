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
import { solve } from "yalps";
import * as U from "./utils.js";
const dayNumber = 12;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n"); // Read and filter empty lines
        // UPDATE FROM HERE
        const shapes = [];
        const grids = [];
        var shape = [];
        for (const line of lines) {
            if (line.trim().length === 0) {
                shapes.push(shape);
                shape = [];
            }
            else if (line.includes("#") || line.includes(".")) {
                const shapeLine = pipe(line.trim().split(""), A.map((c) => c === "#"));
                shape = A.concat(shapeLine)(shape);
            }
            else if (line.includes(":") && line.includes("x")) {
                const parts = line.trim().split(": ");
                const size = pipe(parts[0].split("x"), A.map(parseInt));
                const shapeCounts = pipe(parts[1].split(" "), A.map(parseInt));
                grids.push({ size: [size[0], size[1]], shapeCounts });
            }
        }
        // console.log(`Parsing result shapes: ${JSON.stringify(shapes)}`);
        // console.log(`Parsing result grids: ${JSON.stringify(grids)}`);
        return { shapes, grids };
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const getRotations = (s) => pipe([
    s, // normal
    [s[6], s[3], s[0], s[7], s[4], s[1], s[8], s[5], s[2]], // 90 right
    [s[8], s[7], s[6], s[5], s[4], s[3], s[2], s[1], s[0]], // 180
    [s[2], s[5], s[8], s[1], s[4], s[7], s[0], s[3], s[6]], // 90 left
    [s[2], s[1], s[0], s[5], s[4], s[3], s[8], s[7], s[6]], // mirror
    [s[0], s[3], s[6], s[1], s[4], s[7], s[2], s[5], s[8]], // mirror 90 right
    [s[6], s[7], s[8], s[3], s[4], s[5], s[0], s[1], s[2]], // mirror 180
    [s[8], s[5], s[2], s[7], s[4], s[1], s[6], s[3], s[0]], // mirror 90 left
], A.uniq(A.getEq(B.Eq)));
const getPixels = (shape) => (offset) => pipe(shape, A.mapWithIndex((i, b) => b
    ? O.some([(i % 3) + offset[0], Math.floor(i / 3) + offset[1]])
    : O.none), A.compact);
const combinations = (as) => (bs) => pipe(as, A.flatMap((a) => pipe(bs, A.map((b) => [a, b]))));
const mkModel = (shapes) => (grid) => {
    const startingPos = combinations(NEA.range(0, grid.size[0] - 3))(NEA.range(0, grid.size[1] - 3));
    const variables = pipe(shapes, A.mapWithIndex((shape_idx, shapeRotations) => pipe(shapeRotations, combinations(startingPos), A.mapWithIndex((rotation_idx, [offset, shape]) => pipe(offset, getPixels(shape), (pixels) => [
        `s_${shape_idx}_${rotation_idx}`,
        pipe(pixels, A.map((p) => [`p_${p[0]}_${p[1]}`, 1]), // pixel variables
        A.concat([
            [`sc_${shape_idx}`, 1], // shape totals
            ["obj", 1], // objective
        ]), R.fromEntries),
    ])))), (A.flatten), R.fromEntries);
    const pixelConstraints = pipe(NEA.range(0, grid.size[0] - 1), A.flatMap((x) => pipe(NEA.range(0, grid.size[1] - 1), A.map((y) => [`p_${x}_${y}`, { max: 1 }]))));
    const shapeContraints = pipe(grid.shapeCounts, A.mapWithIndex((i, v) => [
        `sc_${i}`,
        { min: v, max: v },
    ]));
    const constraints = pipe(pixelConstraints, A.concat(shapeContraints), R.fromEntries);
    const integers = pipe(variables, R.keys);
    // console.log(`[DEBUG] Constraints: ${JSON.stringify(constraints)}`);
    // console.log(`[DEBUG] Variables: ${JSON.stringify(variables)}`);
    // console.log(`[DEBUG] Integers: ${JSON.stringify(integers)}`);
    console.log("Starting solve");
    return {
        direction: "maximize",
        objective: "obj",
        constraints,
        variables,
        integers,
    };
};
const preCheck = (grid) => pipe(grid.shapeCounts, concatAll(N.MonoidSum), (nShapes) => nShapes * 9 <= grid.size[0] * grid.size[1]
    ? "Trivial"
    : nShapes * 7 > grid.size[0] * grid.size[1]
        ? "Impossible"
        : "Maybe");
const partA = (parsed) => pipe(parsed.shapes, A.map(getRotations), (rots) => pipe(parsed.grids, A.map((grid) => pipe(grid, preCheck, (check) => check === "Maybe"
    ? pipe(grid, mkModel(rots), solve, (res) => {
        const x = res.status === "optimal" ? 1 : 0;
        console.log(`Solution Status: ${res.status}`);
        // console.log(`Solution: ${JSON.stringify(res.variables)}`);
        return x;
    })
    : check === "Impossible"
        ? 0
        : 1))), concatAll(N.MonoidSum));
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
//# sourceMappingURL=day12.js.map