import fs from "fs/promises";
import { execFile } from "child_process";
import { promisify } from "util";

// quick ANSI escape wrapper
const term = {
  _wrap(code, str) {
    return `\x1b[${code}m${str}\x1b[0;0m`;
  },
  red(str) {
    return this._wrap(31, str);
  },
  green(str) {
    return this._wrap(32, str);
  },
  yellow(str) {
    return this._wrap(33, str);
  },
  blue(str) {
    return this._wrap(34, str);
  },
};

const expectedOutputPattern = /\/\/ expect: ?(.*)/;

const binary = process.argv[2];
const exec = promisify(execFile);

/**
 * @param {string} input
 * @param {string} path
 * @returns {{expectedOutputs: string[]}}}
 */
function parseTest(input, path) {
  const lines = input.split("\n");
  const expectedOutputs = [];
  for (const line of lines) {
    const expectedOutput = expectedOutputPattern.exec(line);
    if (expectedOutput) {
      expectedOutputs.push(expectedOutput[1]);
    }
  }

  return { expectedOutputs };
}

function printTest(test) {
  const status =
    test.status == "passed" ? term.green(test.status) : term.red(test.status);

  console.log(`${test.path}: ${status}`);
  for (const failure of test.failures) {
    const line = `    at line ${term.blue(failure.line)}:`;
    console.log(line);
    console.log(`\texpected: ${term.yellow(failure.expected)}`);
    console.log(`\tfound:    ${term.yellow(failure.actual)}\n`);
  }
}

/**
 *
 * @param {string} path
 */
async function runTest(path) {
  const file = await fs.readFile(path);

  const { expectedOutputs } = parseTest(file.toString());
  const { stdout } = await exec(binary, [path]);

  const actualOutputs = stdout.split("\n");
  const failures = [];

  for (let i = 0; i < expectedOutputs.length; i++) {
    const expected = expectedOutputs[i];
    const actual = actualOutputs[i];

    if (expected.trim() !== actual.trim()) {
      failures.push({ expected, actual, line: i + 1 });
    }
  }

  return {
    status: failures.length ? "failed" : "passed",
    failures,
    path,
  };
}

async function runTests() {
  const tests = [];
  for (const arg of process.argv.slice(3)) {
    tests.push(runTest(arg));
  }

  const results = await Promise.all(tests);

  const passed = results.filter((r) => r.status == "passed");
  const failed = results.filter((r) => r.status == "failed");

  console.log(
    `${results.length} tests ran: ${term.green(
      passed.length
    )} passed, ${term.red(failed.length)} failed`
  );

  console.log();

  for (const test of passed) {
    printTest(test);
  }

  for (const test of failed) {
    printTest(test);
  }

  if (failed.length > 0) {
    process.exit(1);
  }
}

runTests();
