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
undivert(`mini.js')
//]]>

    {
        const puzzles_json = undivert(`puzzles.json');

        var app = Elm.App.init({
            flags: {
                'hasMouse': window.matchMedia('(pointer:fine)').matches,
                'puzzles': puzzles_json.puzzles,
            }
        });
    }
</script>
</body>
</html>
