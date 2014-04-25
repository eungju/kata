import sys
from StringIO import StringIO
import unittest

class Airport:
    def __init__(self, aNumberOfCities):
        self.passengers = []
        for i in range(aNumberOfCities):
            self.passengers.append([0] * aNumberOfCities)
        self.configurations = []
    def setPassenger(self, aArrivalCity, aDepartureCity, aNumber):
        self.passengers[aArrivalCity - 1][aDepartureCity - 1] = aNumber
    def getPassenger(self, aArrivalCity, aDepartureCity):
        return self.passengers[aArrivalCity - 1][aDepartureCity - 1]
    def gateDistance(self, aArrivalGate, aDepartureGate):
        return 1 + abs(aDepartureGate - aArrivalGate)
    def cityDistance(self, aConfiguration, aArrivalCity, aDepartureCity):
        return self.gateDistance(aConfiguration.getArrivalGateFor(aArrivalCity), aConfiguration.getDepartureGateFor(aDepartureCity))
    def calculateTraffic(self, aConfiguration):
        n = len(self.passengers)
        traffic = 0
        for arrivalCity in range(1, n + 1):
            for departureCity in range(1, n + 1):
                traffic = traffic + self.cityDistance(aConfiguration, arrivalCity, departureCity) * self.getPassenger(arrivalCity, departureCity)
        aConfiguration.load = traffic            
        return traffic
    def addConfiguration(self, aConfiguration):
        self.configurations.append(aConfiguration)
    def run(self):
        def compare(c1, c2):
            return cmp(c1.load, c2.load)
        for c in self.configurations:
            self.calculateTraffic(c)
        self.configurations.sort(compare)
        return self.configurations
    
class Configuration:
    def __init__(self, aId, aArrivalCities, aDepartureCities):
        self.id = aId
        self.arrivalCities = aArrivalCities
        self.departureCities = aDepartureCities
    def getArrivalGateFor(self, aCity):
        return self.arrivalCities.index(aCity) + 1
    def getDepartureGateFor(self, aCity):
        return self.departureCities.index(aCity) + 1

def main(stdin, stdout):
    while True:
        line = stdin.readline()
        numOfCities = int(line)
        if numOfCities == 0: break
        airport = Airport(numOfCities)
        for i in range(numOfCities):
            line = stdin.readline()
            token = line.split()
            arrivalCity = int(token[0])
            n = int(token[1])
            for j in range(n):
                departureCity = int(token[2 + j * 2])
                passengers = int(token[2 + j * 2 + 1])
                airport.setPassenger(arrivalCity, departureCity, passengers)
        while True:
            line = stdin.readline()
            id = int(line)
            if id == 0: break
            arrivalCities = map(lambda c: int(c), stdin.readline().split())
            departureCities = map(lambda c: int(c), stdin.readline().split())
            airport.addConfiguration(Configuration(id, arrivalCities, departureCities))
        result = airport.run()
        print >>stdout, 'Configuration Load'
        for c in result:
            print >>stdout, '%5d         %-4d' % (c.id, c.load)
        
class AirportTest(unittest.TestCase):
    def testGateDistance(self):
        airport = Airport(3)
        self.assertEquals(1, airport.gateDistance(1, 1))
        self.assertEquals(2, airport.gateDistance(1, 2))
        self.assertEquals(2, airport.gateDistance(2, 3))
        self.assertEquals(3, airport.gateDistance(3, 1))
    def testSetPassenger(self):
        airport = Airport(3)
        airport.setPassenger(1, 2, 100)
        self.assertEquals(100, airport.getPassenger(1, 2))
        self.assertEquals(0, airport.getPassenger(1, 3))
    def testCityDistance(self):
        airport = Airport(3)
        configuration = Configuration(1, [3, 2, 1], [3, 1, 2])
        self.assertEquals(1, configuration.getArrivalGateFor(3))
        self.assertEquals(3, configuration.getDepartureGateFor(2))
        self.assertEquals(2, airport.cityDistance(configuration, 3, 1))
    def testCalculateTraffic(self):
        airport = Airport(3)
        airport.setPassenger(1, 2, 10)
        airport.setPassenger(1, 3, 15)
        airport.setPassenger(2, 3, 10)
        airport.setPassenger(3, 1, 12)
        airport.setPassenger(3, 2, 20)
        configuration = Configuration(1, [1, 2, 3], [2, 3, 1])
        self.assertEquals(122, airport.calculateTraffic(configuration))
    def testRun(self):
        airport = Airport(3)
        airport.setPassenger(1, 2, 10)
        airport.setPassenger(1, 3, 15)
        airport.setPassenger(2, 3, 10)
        airport.setPassenger(3, 1, 12)
        airport.setPassenger(3, 2, 20)
        c1 = Configuration(1, [1, 2, 3], [2, 3, 1])
        c2 = Configuration(2, [2, 3, 1], [3, 2, 1])
        airport.addConfiguration(c1)
        airport.addConfiguration(c2)
        self.assertEquals([c2, c1], airport.run())
    def testMain(self):
        input = """3
1 2 2 10 3 15
2 1 3 10
3 2 1 12 2 20
1
1 2 3
2 3 1
2
2 3 1
3 2 1
0
2
1 1 2 100
2 1 1 200
1
1 2
1 2
2
1 2
2 1
0
0
"""
        output = StringIO()
        main(StringIO(input), output)
        output = StringIO(output.getvalue())
        output.readline()
        id, load = output.readline().split()
        self.assertEquals(2, int(id))
        self.assertEquals(119, int(load))
        id, load = output.readline().split()
        self.assertEquals(1, int(id))
        self.assertEquals(122, int(load))
        output.readline()
        id, load = output.readline().split()
        self.assertEquals(2, int(id))
        self.assertEquals(300, int(load))
        id, load = output.readline().split()
        self.assertEquals(1, int(id))
        self.assertEquals(600, int(load))
        
if __name__ == '__main__':
    #main()
    unittest.main(argv=('', '-v'))
