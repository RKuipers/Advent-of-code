import * as A from "fp-ts/lib/Array.js";
import * as E from "fp-ts/lib/Either.js";
import { flow, pipe } from "fp-ts/lib/function.js";
import * as O from "fp-ts/lib/Option.js";
import * as R from "fp-ts/lib/Record.js";
import * as fs from "fs/promises"; // Use promises for asynchronous file reading
import * as path from "path";
import { modulo } from "./utils.js";
const dayNumber = 1;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const llist = [];
        const rlist = [];
        for (const line of lines) {
            const parts = line.trim().split(/\s+/); // Split by one or more spaces
            if (parts.length === 2) {
                llist.push(parseInt(parts[0], 10));
                rlist.push(parseInt(parts[1], 10));
            }
        }
        return { llist, rlist };
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const partA = (parsed) => {
    // USE parsed.x if parsed type is an object
    return 0;
};
// --- Part B Logic ---
const partB = (parsed) => {
    // USE parsed.x if parsed type is an object
    return 0;
};
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
    const y = flow((x) => modulo(x, 5));
};
//# sourceMappingURL=template.js.map