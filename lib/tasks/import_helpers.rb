
def node_text(plas, name)

  ch = plas.xpath(name)[0].child

  return "" unless ch

  ch.text

end

def parse_date(date)

  if date.length > 0 then
    if /\d{1,2}\/\d{1,2}\/\d{4}/.match(date) then
      date=Date.strptime(date, '%m/%d/%Y')
    elsif /\d{1,2}-\d{1,2}-\d{4}/.match(date) then
      date=Date.strptime(date, '%m-%d-%Y')
    elsif /\d{4}-\d{1,2}-\d{1,2}/.match(date) then
      date=Date.strptime(date, '%Y-%m-%d')
    elsif /\d{4}\/\d{1,2}\/\d{1,2}/.match(date) then
      date=Date.strptime(date, '%Y/%m/%d')
    end
  end

  date

end