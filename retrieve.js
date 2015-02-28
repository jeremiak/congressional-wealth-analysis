var fs = require('fs');

var request = require('request');

function downloadYear(year) {
  var base = 'http://www.opensecrets.org/db2dl/?q=PFDsWorth&output=CSV', 
      year = year || '2013',
      fullUrl = base + '&cycle=' + year,
      filePath = 'data/' + year + '.csv';
  request(fullUrl).pipe(fs.createWriteStream(filePath));
}

function downloadAllYears() {
  var years = ['2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013'];

  years.map(function(year){
    downloadYear(year);
  });
}

downloadAllYears();
