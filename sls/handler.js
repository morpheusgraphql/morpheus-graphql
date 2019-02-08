'use strict';

const {
    spawn
} = require('child_process');

const destination = '.stack-work/install/x86_64-osx/nightly-2018-09-28/8.4.3/bin/bff'
const badJson = output => 'child process output bad JSON: ' + output
const handleError = (callback, message) => {
    console.error(message);
    return message;
}


exports.bff = (event, _, callback) => {

    process.env['PATH'] = process.env['PATH'] + ':' +
        process.env['LAMBDA_TASK_ROOT'];

    const main = spawn(destination, [] /*exec arguments*/ , {
        stdio: ['pipe', process.stdout, process.stderr, 'pipe'],
    });

    const stdin = main.stdin;
    const communication = main.stdio[3];

    // Send the event to the process
    stdin.end(JSON.stringify(event) + '\n', 'utf8');

    // Keep track of the process output
    let output = '';
    communication.on('data', chunk => output += chunk);

    let exited = false;

    main.on('exit', code => {
        if (!exited) {
            exited = true;
            if (code == 0) {
                try {
                    const result = JSON.parse(output);
                    callback(null, result);
                } catch (err) {
                    callback(handleError(badJson(output)))
                }
            } else {
                callback(handleError('child process exited with code ' + code))
            }
        }
    });

    main.on('error', err => {
        if (!exited) {
            exited = true;
            console.error('error: ' + err);
            callback(err, 'child process exited with error: ' + err);
        }
    });
};