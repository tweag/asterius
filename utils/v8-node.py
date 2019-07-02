#!/usr/bin/env python3

from io import BytesIO
from html.parser import HTMLParser
import os
from urllib.request import urlopen
from zipfile import ZipFile


def get_bytes(url):
    return urlopen(url).read()


def get_str(url):
    return get_bytes(url).decode()


class P0(HTMLParser):
    def __init__(self):
        super().__init__()
        self.gate = False
        self.success_builds = []

    def handle_starttag(self, tag, attrs):
        if attrs == [("class", "status SUCCESS")]:
            self.gate = True
        elif self.gate and tag == "a":
            self.success_builds.append("https://ci.chromium.org" + attrs[0][1])
            self.gate = False


class P1(HTMLParser):
    def __init__(self):
        super().__init__()
        self.download_link = None

    def handle_starttag(self, tag, attrs):
        if len(attrs) >= 3 and attrs[2][1].startswith("https://storage.googleapis.com/") and attrs[2][1].endswith(
                ".zip"):
            self.download_link = attrs[2][1]


def get_node_zip_url():
    p0 = P0()
    p0.feed(get_str("https://ci.chromium.org/p/v8/builders/luci.v8.ci/V8%20Linux64%20-%20node.js%20integration%20ng"))
    p1 = P1()
    p1.feed(get_str(p0.success_builds[0]))
    return p1.download_link


def extract_node_zip(node_zip_bytes, path):
    with ZipFile(BytesIO(node_zip_bytes)) as z:
        for i in z.infolist():
            z.extract(i.filename, path)
            os.chmod(os.path.join(path, i.filename), i.external_attr >> 16)


if __name__ == "__main__":
    extract_node_zip(get_bytes(get_node_zip_url()), os.getcwd())
