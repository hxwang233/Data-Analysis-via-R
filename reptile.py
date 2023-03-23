import requests
import csv
import json
from bs4 import BeautifulSoup

if __name__ == '__main__':
    header = ['代码', '名称', '基本每股收益(元)', '每股净资产', '每股经营活动产生的现金流量净额', '主营业务收入', '主营业务利润', '营业利润', '投资收益', '营业外收支净额',
              '利润总额', '净利润', '净利润(扣除非经常性损益后)', '经营活动产生的现金流量净额', '现金及现金等价物净增加额', '总资产', '流动资产', '总负债', '流动负债',
              '股东权益不含少数股东权益', '净资产收益率加权', '总资产利润率', '主营业务利润率', '总资产净利润率', '成本费用利润率', '营业利润率', '主营业务成本率', '销售净利率',
              '净资产收益率', '股本报酬率', '净资产报酬率', '资产报酬率', '销售毛利率', '三项费用比重', '非主营比重', '主营利润比重', '流动比率', '速动比率', '现金比率',
              '利息支付倍数', '资产负债率', '长期债务与营运资金比率', '股东权益比率', '长期负债比率', '股东权益与固定资产比率', '负债与所有者权益比率', '长期资产与长期资金比率', '资本化比率',
              '固定资产净值率', '资本固定化比率', '产权比率', '清算价值比率', '固定资产比重', '主营业务收入增长率', '净利润增长率', '净资产增长率', '总资产增长率', '应收账款周转率',
              '应收账款周转天数', '存货周转率', '固定资产周转率', '总资产周转率', '存货周转天数', '总资产周转天数', '流动资产周转率', '流动资产周转天数', '经营现金净流量对销售收入比率',
              '资产的经营现金流量回报率', '经营现金净流量与净利润的比率', '经营现金净流量对负债比率', '现金流量比率', '所属模块', '地域']
    print(len(header))
    with open('./data.csv', 'w', encoding='utf-8') as f:
        f_csv = csv.writer(f)
        f_csv.writerow(header)
        modules = ['hy003023', 'hy003014', 'hy003026', 'hy007000']
        moduleNames = ['汽车制造', '医药制造', '通信设备', '交通物流']
        pages = [6, 10, 16, 5]
        # 遍历四个模块
        for i in range(4):
            # 根据每个模块的页数进行遍历
            for page in range(pages[i]):
                url = 'http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=' + str(
                    page) + '&query=PLATE_IDS%3A' + modules[
                          i] + '&fields=NO%2CSYMBOL%2CNAME%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=PERCENT&order=desc&count=24&type=query'
                req = requests.get(url=url)
                jsonObj = json.loads(req.text)
                datalist = jsonObj['list']
                for data in datalist:
                    row = []
                    row.append(data['SYMBOL'])
                    row.append(data['NAME'])
                    # 根据Symbol得到对应的详情页
                    data_detail_url = 'http://quotes.money.163.com/f10/zycwzb_' + data['SYMBOL'] + '.html#01c01'
                    detail_req = requests.get(url=data_detail_url)
                    # 创建BeautifulSoup对象
                    soup = BeautifulSoup(detail_req.text, "lxml")
                    main_table = soup.find('table', class_="scr_table")
                    main_trs = main_table.find_all("tr")
                    del(main_trs[0])
                    for tr in main_trs:
                        td = tr.find_all("td",limit=1)[0].string
                        if td == "--":
                            row.append('?')  # 缺失值记为?
                        elif td.find(",") != -1:
                            row.append(td.replace(',', ''))  # 处理含有逗号的数字，含有逗号则去掉
                        else:
                            row.append(td)
                    tables = soup.find_all('table', class_="fund_analys")
                    for table in tables:
                        trs = table.find_all("tr")
                        del (trs[0])  # 去掉表头
                        for tr in trs:
                            td = tr.find_all("td", limit=2)[1].string  # 取第二列
                            if td == "--":
                                row.append('?')  # 缺失值记为?
                            elif td.find(",") != -1:
                                row.append(td.replace(',', ''))  # 处理含有逗号的数字，含有逗号则去掉
                            else:
                                row.append(td)
                    row.append(moduleNames[i])
                    # 爬取地域
                    data_area_url = 'http://quotes.money.163.com/f10/gszl_' + data['SYMBOL'] + '.html#01f01'
                    area_req = requests.get(url=data_area_url)
                    soup2 = BeautifulSoup(area_req.text, "lxml")
                    table = soup2.find_all('table', class_="table_details")[0]
                    tr = table.find_all("tr", limit=1)[0]
                    area = tr.find_all("td")[3].string
                    if area is None:
                        row.append("?")  # 处理缺失值
                    else:
                        row.append(area)
                    # 将row写入文件
                    f_csv.writerow(row)
                    print(row)
