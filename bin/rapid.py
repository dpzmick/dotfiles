#!/usr/bin/env python3

from http.server import SimpleHTTPRequestHandler, HTTPServer
from string import Template
import os
import socket

# RAPID: Real-time Advanced Plotting and Interactive Display.

template_str = r"""
<!DOCTYPE html>
<head>
<title>$path</title>

<style type="text/css">
body {
   margin: 0;
   overflow: hidden;
}
#frame {
    position:absolute;
    left: 0px;
    width: 100%;
    top: 0px;
    height: 100%;
}
</style>
<script>

var mtime = "$initMtime";
function checkMtime() {
  fetch('http://$ip:$port/__mtime/$path')
    .then(response => {
      if (!response.ok) {
        throw new Error('Network response was not ok');
      }
      return response.json();
    })
    .then(data => {
      if (data['mtime'] !== mtime) {
        mtime = data['mtime'];
        document.getElementById('frame').contentWindow.location.reload();
      }
    })
    .catch(error => {
      console.error('Error:', error);
    });
}

const requestInterval = 1000;
const timerId = setInterval(checkMtime, requestInterval);

</script>
</head>

<body>
<iframe id="frame" src="http://$ip:$port/__internal/$path"></iframe>

</body>
</html>
"""

html_template = Template(template_str)

class RapidHandler(SimpleHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/":
            return super().do_GET()
        elif self.path.startswith('/__internal/'): # forward to static/original path
            self.path = self.path[len('__internal/'):]
            return super().do_GET()
        elif self.path.startswith('/__mtime/'):
            path = self.path[len('__mtime/'):]
            file_path = self.server.directory + '/' + path
            mtime = os.path.getmtime(file_path)

            self.send_response(200)
            # do not cache results (for images and stuff)
            self.send_header('Cache-Control', 'no-cache, no-store, must-revalidate')
            self.send_header('Pragma', 'no-cache')
            self.send_header('Expires', '0')
            self.end_headers()
            self.wfile.write(f'{{"mtime": "{mtime}"}}'.encode('utf-8'))
        else:
            file_path = self.server.directory + '/' + self.path
            mtime = os.path.getmtime(file_path)

            result = html_template.substitute(
                ip=self.server.host,
                port=self.server.port,
                path=self.path,
                initMtime=mtime)

            self.send_response(200)
            self.end_headers()
            self.wfile.write(result.encode('utf-8'))

if __name__ == "__main__":
    host = socket.gethostname()
    port = 5069

    print(f'Listening on http://{host}:{port}')

    server = HTTPServer((host, port), RapidHandler)
    # are these already there? who knows
    server.host = host
    server.port = port
    server.directory = os.getcwd()
    server.serve_forever()
