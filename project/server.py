import sys
import urllib
import urllib2
import json
from time import time

from twisted.python import log
from twisted.internet import reactor
from twisted.internet.protocol import ServerFactory, ClientFactory, Protocol

import conf

# basic structure from:
# http://www.codeghar.com/blog/echo-server-and-client-with-twisted.html

# set debug to true to log to stdout instead of to a log file
DEBUG = True


class ServerConfig(object):

    def __init__(self, port, peers):
        self.port = port
        self.peers = peers


# "talk" is bidirectional; cf. https://piazza.com/class/ij4pi2k5m0d5gn?cid=290
SERVER_CONFIG = {
    'Alford': ServerConfig(port=conf.PORT_NUM['ALFORD'],
                           peers=['Hamilton', 'Welsh']),
    'Ball': ServerConfig(port=conf.PORT_NUM['BALL'],
                         peers=['Holiday', 'Welsh']),
    'Hamilton': ServerConfig(port=conf.PORT_NUM['HAMILTON'],
                             peers=['Alford', 'Holiday']),
    'Holiday': ServerConfig(port=conf.PORT_NUM['HOLIDAY'],
                            peers=['Ball', 'Hamilton']),
    'Welsh': ServerConfig(port=conf.PORT_NUM['WELSH'],
                          peers=['Alford', 'Ball'])
}


class Client(object):
    '''Represents a client that has connected to a Places server in the herd.
    '''

    def __init__(self, location, timestamp, time_difference):
        self.location = location
        self.timestamp = timestamp
        self.time_difference = time_difference

    def __str__(self):
        return "%s at %s (skew %s)" % (self.location, self.timestamp,
                                       self.time_difference)


class PlacesServerProtocol(Protocol):
    '''Protocol for servers to handle place query and update requests.
    '''
    base_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
    api_key = conf.API_KEY

    def __init__(self, server):
        self.server = server

    def dataReceived(self, data):
        '''Executed upon receiving a TCP request.'''
        log.msg('[server] Data received: {}'.format(data))
        args = data.split(' ')
        command = args[0]
        if command == "IAMAT" and len(args) == 4:
            loc = self.parse_client_location(args[1:])
            if loc is None: # parse error
                return self.send_error(data)
            self.iamat(args, loc['client'], loc['coords'], loc['timestamp'])
        elif command == "AT" and len(args) == 6:
            loc = self.parse_client_location(args[3:])
            self.at(data, args[1], loc['client'], loc['coords'],
                    loc['timestamp'], float(args[2]))
        elif command == "WHATSAT" and len(args) == 4:
            client = args[1]
            try:
                radius = int(args[2])
                num_results = int(args[3])
            except ValueError:
                self.send_error(data)
            if radius > 50 or num_results > 20:
                self.send_error(data)
            self.whatisat(args, client, radius, num_results)
        else: # unrecognized command
            log.msg('[server] Malformed command: {}'.format(data))
            self.send_error(data)

    def parse_client_location(self, args):
        '''parses a client location string of the format "[name] [coordinates]
           [timestamp]" into a dict with keys "client", "coords", and
           "timestamp". Returns None on failure.
        '''
        loc = {}
        loc['client'] = args[0]
        loc['coords'] = args[1]
        try:
            loc['timestamp'] = float(args[2])
        except ValueError:
            return None
        return loc

    def format_iamat(self, args, client, location, timestamp, 
                     time_difference):
        return "AT %s %s %s" % (self.server.name, time_difference, 
                                ' '.join(args[1:]))

    def format_location(self, coordinates):
        '''formats coordinates as "lat,long" for Places API'''
        return coordinates.replace('+', '').replace('-', ',')
    
    def iamat(self, args, client, location, timestamp):
        time_difference = time() - timestamp
        self.server.clients[client] = Client(location, timestamp, 
                                             time_difference)
        self.log_clients()
        response = self.format_iamat(args, client, location, timestamp,
                                     time_difference)
        self.transport.write(response)
        self.propagate(response)
    
    def at(self, data, origin_server, client, location, timestamp, 
           time_difference):
        # the server already has the location information for the given client
        # at the given time, do nothing
        if self.duplicate(client, timestamp):
            return
        self.server.clients[client] = Client(location, timestamp, time_difference)
        self.log_clients()
        self.propagate(data, origin_server)

    def whatisat(self, args, client_name, radius, num_results):
        client = self.server.clients[client_name]
        # https://developers.google.com/places/web-service/search#PlaceSearchRequests
        query = urllib.urlencode({
            'key': self.api_key,
            'location': self.format_location(client.location),
            'radius': radius
        })
        url = self.base_url + query
        response = urllib2.urlopen(url).read()
        data = json.loads(response)
        # limit results to the first `num_results` results
        data['results'] = data['results'][:num_results]
        at_response = self.format_iamat(args, client_name, client.location, 
                                        client.timestamp, 
                                        client.time_difference)
        self.transport.write(
            # API response is indented w/ 3 spaces because eggert makes his 
            # own rules
            # use two empty strings to get two newlines at the end per spec
            '\n'.join([at_response, json.dumps(data, indent=3), '', ''])
        )

    def duplicate(self, client, timestamp):
        '''Checks if the given client at the current timestamp is already in
        memory.
        '''
        return (self.server.clients.get(client) and
                self.server.clients[client].timestamp == timestamp)

    def propagate(self, payload, origin_server=None):
        # iterate through a list of this server's peers that does not include
        # the server from which we got the message (origin_server), if any
        server_peers = SERVER_CONFIG[self.server.name].peers
        for server in ([SERVER_CONFIG[s] for s in server_peers if
                        s != origin_server]):
            log.msg('Propagating payload to server at {}'.format(server.port))
            reactor.connectTCP('localhost', server.port,
                               PlacesClientFactory(payload))

    def send_error(self, data):
        return self.transport.write('? %s' % data)

    def log_clients(self):
        log.msg('Known clients: {}'.format(
            ['%s: %s' % (key, self.server.clients[key]) for key in \
             self.server.clients]
        ))

    def connectionMade(self):
        log.msg('[server] Client connected: {}'.format(self.transport.getPeer()))

    def connectionLost(self, reason):
        log.msg('[server] Connection closed: {}'.format(reason))


class PlacesClientProtocol(Protocol):

    def __init__(self, server):
        self.server = server

    def dataReceived(self, data):
        log.msg('[client] Data received: {}'.format(data))
        self.transport.loseConnection()

    def connectionMade(self):
        # data = 'Hello, Server!'
        # self.transport.write(data.encode())
        log.msg('[client] Connection established: {}'\
                .format(self.server.payload))
        self.transport.write(self.server.payload.encode())
        self.transport.loseConnection()

    def connectionLost(self, reason):
        log.msg('[client] Connection closed: {}'.format(reason))


class PlacesServerFactory(ServerFactory):
    clients = {}

    def __init__(self, name):
        self.name = name

    def buildProtocol(self, addr):
        return PlacesServerProtocol(self)


class PlacesClientFactory(ClientFactory):

    def __init__(self, payload):
        self.payload = payload

    def startedConnecting(self, connector):
        log.msg('[client] Started to connect.')

    def buildProtocol(self, addr):
        log.msg('[client] Connected to: {}'.format(addr))
        return PlacesClientProtocol(self)

    def clientConnectionLost(self, connector, reason):
        log.msg('[client] Connection closed: {}'.format(reason))

    def clientConnectionFailed(self, connector, reason):
        log.msg('[client] Connection failed: {}'.format(reason))


def main():
    try:
        server_name = sys.argv[1]
    except IndexError:
        sys.exit('required server argument missing\n'
                 'usage: python server.py [server name]')
    if DEBUG:
        log_location = sys.stdout
    else:
        log_location = open('%s.log' % server_name, 'w')
    log.startLogging(log_location)
    server = SERVER_CONFIG[server_name]
    reactor.listenTCP(server.port, PlacesServerFactory(server_name))
    reactor.run()
    log.msg('[server] Started {} on port {}'.format(server_name, server.port))

if __name__ == '__main__':
    main()