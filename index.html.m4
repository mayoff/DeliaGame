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
                'puzzles': puzzles_json.puzzles,
            }
        });

        function keyForDate(date) { return `text-${date}`; }

        app.ports.javascriptRequest.subscribe(function (request) {
            switch (request.type) {
                case 'SetText':
                    localStorage.setItem(keyForDate(request.date), request.text);
                    break;

                case 'GetText':
                    const text = localStorage.getItem(keyForDate(request.date));
                    app.ports.javascriptResponse.send({
                        date: request.date,
                        text: text,
                        type: 'DidGetText'
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
