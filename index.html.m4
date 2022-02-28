<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width">
    <title>Delia's Game</title>
</head>
<body>
<script>
//<![CDATA[
changequote(`[[', `]]');
undivert([[mini.js]])
//]]>

    {
        'use strict';

        const puzzles_json = undivert([[puzzles.json]]);

        const app = Elm.App.init({
            flags: {
                'hasMouse': window.matchMedia('(pointer:fine)').matches,
                'isoDateTime': ifdef([[debug]],[['9999-12-31T23:59:59.999Z']],[[new Date().toISOString()]]),
                'puzzles': puzzles_json.puzzles,
            }
        });

        app.ports.javascriptRequest.subscribe(function (request) {
            switch (request.type) {
                case 'LocalStorageSet':
                    localStorage.setItem(request.key, request.value);
                    break;

                case 'LocalStorageGet':
                    const value = localStorage.getItem(request.key);
                    app.ports.javascriptResponse.send({
                        key: request.key,
                        type: 'LocalStorageDidGet',
                        value: value
                    });
                    break;

                default:
                    break;
            }
        });
    }
</script>
</body>
</html>
