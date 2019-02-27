from html.parser import HTMLParser
from urllib.request import urlopen


def get(url):
    r = urlopen(url)
    return r.read().decode(r.info().get_content_charset())


class P0(HTMLParser):
    def __init__(self):
        super().__init__()
        self.__gate = False
        self.success_builds = []

    def handle_starttag(self, tag, attrs):
        if attrs == [("class", "status-Success")]:
            self.__gate = True
        elif self.__gate and tag == "a":
            self.success_builds.append("https://ci.chromium.org" + attrs[0][1])
            self.__gate = False


class P1(HTMLParser):
    def __init__(self):
        super().__init__()
        self.download_link = None

    def handle_starttag(self, tag, attrs):
        if len(attrs) >= 2 and attrs[1][1] == "step link for download":
            self.download_link = attrs[0][1]


if __name__ == "__main__":
    p0 = P0()
    p0.feed(get("https://ci.chromium.org/p/v8/builders/luci.v8.ci/V8%20Linux64%20-%20node.js%20integration?limit=200"))
    p1 = P1()
    p1.feed(get(p0.success_builds[0]))
    print(p1.download_link, end="")
