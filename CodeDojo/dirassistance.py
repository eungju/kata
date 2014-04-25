import unittest

class DirectoryAssistant:
    SIGNATURES = {
        'ABC': '2',
        'DEF': '3',
        'GHI': '4',
        'JKL': '5',
        'MNO': '6',
        'PRS': '7',
        'TUV': '8',
        'WXY': '9'
    }
    def __init__(self):
        self.phoneNumbers = []
    def signature(self, name):
        sig = ''
        for c in filter(lambda c: c.isalpha() and not in ['Q', 'Z'], name.upper()):
            for g in DirectoryAssistant.SIGNATURES.keys():
                if c in g:
                    sig = sig + DirectoryAssistant.SIGNATURES[g]
        return sig
    def add(self, name, phoneNumber):
        self.phoneNumbers.append((name, phoneNumber))
    def suggest(self, sig):
        entries = []
        for e in self.phoneNumbers:
            if self.signature(e[0]).startswith(sig):
                entries.append(e)
        return entries
    
class DirectoryAssitantTest(unittest.TestCase):
    def testSignature(self):
        assistant = DirectoryAssistant()
        self.assertEquals('7', assistant.signature('P'))
        self.assertEquals('7', assistant.signature('R'))
        self.assertEquals('2', assistant.signature('A'))
        self.assertEquals('2', assistant.signature('a'))
        self.assertEquals('7275', assistant.signature('PARK'))
    def testSuggest(self):
        eungju = ('Park Eung-ju', '0165829940')
        moonsu = ('Park Moon-su', '00011112222')
        assistant = DirectoryAssistant()
        assistant.add(eungju[0], eungju[1])
        self.assertEquals([eungju], assistant.suggest('7275'))
        assistant.add(moonsu[0], moonsu[1])
        self.assertEquals([eungju, moonsu], assistant.suggest('7275'))
        self.assertEquals([eungju], assistant.suggest('72753'))

class Telephone:
    def __init__(self, assistant):
        self.assistant = assistant
        self.pushed = ''
        self._suggestions = []
    def push(self, button):
        if button in [str(x) for x in range(0, 10)]:
            self.pushed = self.pushed + button
        elif button == '*':
            self._suggestions = self.assistant.suggest(self.pushed)
    def suggestions(self):
        return self._suggestions
    
class TelephoneTest(unittest.TestCase):
    def testPush(self):
        eungju = ('Park Eung-ju', '0165829940')
        moonsu = ('Park Moon-su', '00011112222')
        assistant = DirectoryAssistant()
        assistant.add(eungju[0], eungju[1])
        assistant.add(moonsu[0], moonsu[1])
        telephone = Telephone(assistant)
        telephone.push('7')
        telephone.push('2')
        telephone.push('7')
        telephone.push('5')
        telephone.push('*')
        self.assertEquals([eungju, moonsu], telephone.suggestions())
        telephone.push('3')
        telephone.push('*')
        self.assertEquals([eungju], telephone.suggestions())
        
if __name__ == '__main__':
    unittest.main(argv=('', '-v'))
