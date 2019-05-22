const highlight = require('cli-highlight').highlight;
const formatter = require('sql-formatter');
const getStdin = require('get-stdin');


(async () => {
    const input = await getStdin();
    const formatted = formatter.format(input);
    const highlighted = highlight(formatted,{language:'sql', ignoreIllegals: true});
    console.log(highlighted)
})();

