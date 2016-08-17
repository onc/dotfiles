#! /usr/bin/env python2

import os
import re

import binascii
import codecs

import gnomekeyring as gkey


def set_credentials(repo, user, pw):
    KEYRING_NAME = "offlineimap"
    attrs = {"repo": repo, "user": user}
    keyring = gkey.get_default_keyring_sync()
    gkey.item_create_sync(keyring, gkey.ITEM_NETWORK_PASSWORD,
                          KEYRING_NAME, attrs, pw, True)


def get_credentials(repo):
    keyring = gkey.get_default_keyring_sync()
    attrs = {"repo": repo}
    items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
    return (items[0].attributes["user"], items[0].secret)


def get_username(repo):
    return get_credentials(repo)[0]


def get_password(repo):
    return get_credentials(repo)[1]

if __name__ == "__main__":
    import sys
    import os
    import getpass
    if len(sys.argv) != 3:
        print "Usage: %s <repository> <username>" \
            % (os.path.basename(sys.argv[0]))
        sys.exit(0)
    repo, username = sys.argv[1:]
    password = getpass.getpass("Enter password for user '%s': " % username)
    password_confirmation = getpass.getpass("Confirm password: ")
    if password != password_confirmation:
        print "Error: password confirmation does not match"
        sys.exit(1)
    set_credentials(repo, username, password)

# UTF-7
# from: https://github.com/dpwright/dotfiles/blob/master/.offlineimap.utf7.py


# encoding
def modified_base64(s):
    s = s.encode('utf-16be')
    return binascii.b2a_base64(s).rstrip('\n=').replace('/', ',')


def doB64(_in, r):
    if _in:
        r.append('&%s-' % modified_base64(''.join(_in)))
        del _in[:]


def encoder(s):
    r = []
    _in = []
    for c in s:
        ordC = ord(c)
        if 0x20 <= ordC <= 0x25 or 0x27 <= ordC <= 0x7e:
            doB64(_in, r)
            r.append(c)
        elif c == '&':
            doB64(_in, r)
            r.append('&-')
        else:
            _in.append(c)
    doB64(_in, r)
    return (str(''.join(r)), len(s))


# decoding
def modified_unbase64(s):
    b = binascii.a2b_base64(s.replace(',', '/') + '===')
    return unicode(b, 'utf-16be')


def decoder(s):
    r = []
    decode = []
    for c in s:
        if c == '&' and not decode:
            decode.append('&')
        elif c == '-' and decode:
            if len(decode) == 1:
                r.append('&')
            else:
                r.append(modified_unbase64(''.join(decode[1:])))
            decode = []
        elif decode:
            decode.append(c)
        else:
            r.append(c)
    if decode:
        r.append(modified_unbase64(''.join(decode[1:])))
    bin_str = ''.join(r)
    return (bin_str, len(s))


class StreamReader(codecs.StreamReader):
    def decode(self, s, errors='strict'):
        return decoder(s)


class StreamWriter(codecs.StreamWriter):
    def decode(self, s, errors='strict'):
        return encoder(s)


def imap4_utf_7(name):
    if name == 'imap4-utf-7':
        return (encoder, decoder, StreamReader, StreamWriter)
codecs.register(imap4_utf_7)
