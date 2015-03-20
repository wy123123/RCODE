library(httr)
library(XML)
url="http://103.9.102.202/lbonitoadmin/index.php?tab=AdminCatalog&id_category=19&viewcategory&token=93fdc459ac6334c274d6886c3200689d"
con=GET(url,authenticate("wangyu@lovebonito.com", "ilovebonito"))
pagetree <- htmlTreeParse(con)
node=xmlRoot(pagetree)

xmlName(node)
names(node)

node[[2]]

node=xmlRoot(body)

xmlSApply(pagetree,xmlValue)

body=body <- pagetree$children$html$children$body 

class(html)
??XML
