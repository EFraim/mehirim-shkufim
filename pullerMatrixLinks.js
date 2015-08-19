w = require('webpage')
fs = require('fs')
linksOut = fs.open('matrixLinks.txt', 'w');
page = w.create();

page.onLoadFinished = function(status)
{
    var downloads = page.evaluate(function()
		  {
		      downloads=[];
		      var iter = document.evaluate("//table//td//a", document, null, XPathResult.UNORDERED_NODE_ITERATOR_TYPE, null);
		      for(var link=iter.iterateNext(); link; link=iter.iterateNext())
			  downloads.push(link.href);
		      return downloads;
		  });
    downloads.forEach(function(link) {
	linksOut.writeLine(link);
    });

    if(page.evaluate(function()
		  {
		      var next = document.getElementById('MainContent_btnNext1');
		      if(next.disabled)
			  return false;
		      else
		      {
			  next.click();
			  return true;
		      }
		  }))
	console.log("Next");
    else
    {
	console.log("Done");
	phantom.exit(0);
    }
}

page.open("http://matrixcatalog.co.il/NBCompetitionRegulations.aspx")
