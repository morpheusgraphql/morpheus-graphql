'use strict';

const { spawn } = require('child_process');

function wrapper(options) {
    const executable = options['executable'];
    const execArguments = options['arguments'];
    return function (event, context, callback) {
        process.env['PATH'] = process.env['PATH'] + ':' +
            process.env['LAMBDA_TASK_ROOT'];

        const main = spawn('./' + executable, execArguments, {
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

        main.on('exit', function (code) {
            if (!exited) {
                exited = true;
                if (code == 0) {
                    try {
                        const result = JSON.parse(output);
                        callback(null, result);
                    } catch (err) {
                        console.error('child process output bad JSON: ' + output);
                        callback('child process output bad JSON: ' + output);
                    }
                }
                else {
                    console.error('child process exited with code ' + code);
                    callback('child process exited with code ' + code);
                }
            }
        });

        main.on('error', function (err) {
            if (!exited) {
                exited = true;
                console.error('error: ' + err);
                callback(err, 'child process exited with error: ' + err);
            }
        });
    };
}

// exports such as below will be added here by the plugin
// exports['EXECUTABLENAME'] = wrapper({
//   executable: 'EXECUTABLENAME',
//   arguments: ['--arg1', '--arg2'],
// });
exports['bff'] = wrapper({"executable":"bff","arguments":[]});
