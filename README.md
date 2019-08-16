![ejabberd Read Markers](doc/assets/project.svg)

[![Build Status](https://api.travis-ci.com/hausgold/ejabberd-read-markers.svg?token=4XcyqxxmkyBSSV3wWRt7&branch=master)](https://travis-ci.com/hausgold/ejabberd-read-markers)

This is a custom [ejabberd](https://www.ejabberd.im/) module which allows users
to acknowledge/retrieve their last read message on a multi user conference.
There is also an unseen counter per user per room which gets updated on new
messages on the room. Think of the WhatsApp read message markers, with this
module and a custom client you can implement the same feature. You can find
[further details of the concept](./doc/concept.md) to learn more about the
client usage.  This project comes with a self-contained test setup with all
required parts of the stack.  powerful.

- [Requirements](#requirements)
  - [Runtime](#runtime)
  - [Build and development](#build-and-development)
- [Installation](#installation)
- [Configuration](#configuration)
  - [Database](#database)
- [Development](#development)
  - [Getting started](#getting-started)
    - [mDNS host configuration](#mdns-host-configuration)
  - [Test suite](#test-suite)
- [Additional readings](#additional-readings)

## Requirements

### Runtime

* [ejabberd](https://www.ejabberd.im/) (=18.01)
* [PostgreSQL](https://www.postgresql.org/) (>=9.0)

### Build and development

* [GNU Make](https://www.gnu.org/software/make/) (>=4.2.1)
* [Docker](https://www.docker.com/get-docker) (>=17.09.0-ce)
* [Docker Compose](https://docs.docker.com/compose/install/) (>=1.22.0)

## Installation

See the [detailed installation instructions](./INSTALL.md) to get the ejabberd
module up and running. When you are using Debian/Ubuntu, you can use an
automatic curl pipe script which simplifies the installation process for you.

## Configuration

We make use of the global database settings of ejabberd, but you can also
specify a different database type by setting it explicitly.

```yaml
modules:
  mod_read_markers:
    db_type: sql
```

Keep in mind that this implementation just features the `sql` database type,
and only this.

### Database

The concept outlined the `read_messages` table definition which is required to
store the read messages per user per room. The [actual SQL
schema](./config/postgres/99-pg-read-markers.sql) MUST be executed on
the Jabber service database (PostgreSQL).

## Development

### Getting started

The project bootstrapping is straightforward. We just assume you took already
care of the requirements and you have your favorite terminal emulator pointed
on the project directory.  Follow the instructions below and then relaxen and
watchen das blinkenlichten.

```bash
# Installs and starts the ejabberd server and it's database
$ make start

# (The jabber server should already running now on its Docker container)

# Open a new terminal on the project path,
# install the custom module and run the test suite
$ make reload test
```

When your host mDNS Stack is fine, you can also inspect the [ejabberd admin
webconsole](http://jabber.local/admin) with
`admin@jabber.local` as username and `defaultpw` as password. In the
case you want to shut this thing down use `make stop`.

#### mDNS host configuration

If you running Ubuntu/Debian, all required packages should be in place out of
the box. On older versions (Ubuntu < 18.10, Debian < 10) the configuration is
also fine out of the box. When you however find yourself unable to resolve the
domains or if you are a lucky user of newer Ubuntu/Debian versions, read on.

**Heads up:** This is the Arch Linux way. (package and service names may
differ, config is the same) Install the `nss-mdns` and `avahi` packages, enable
and start the `avahi-daemon.service`. Then, edit the file `/etc/nsswitch.conf`
and change the hosts line like this:

```bash
hosts: ... mdns4 [NOTFOUND=return] resolve [!UNAVAIL=return] dns ...
```

Afterwards create (or overwrite) the `/etc/mdns.allow` file when not yet
present with the following content:

```bash
.local.
.local
```

This is the regular way for nss-mdns > 0.10 package versions (the
default now). If you use a system with 0.10 or lower take care of using
`mdns4_minimal` instead of `mdns4` on the `/etc/nsswitch.conf` file and skip
the creation of the `/etc/mdns.allow` file.

**Further readings**
* Archlinux howto: https://wiki.archlinux.org/index.php/avahi
* Ubuntu/Debian howto: https://wiki.ubuntuusers.de/Avahi/
* Further detail on nss-mdns: https://github.com/lathiat/nss-mdns

### Test suite

The test suite sets up a simple environment with 3 independent users. (admin,
alice and bob). A new test room is created by the admin user, as well as alice
and bob were made members by setting their affiliations on the room. (This is
the same procedure we use on production for lead/user/agent integrations on the
Jabber service) The suite performs then some common tasks on the service:
sending two text messages, acknowledge the last read message of the admin user
and last but not least retrieve the last read message for multiple users. The
database table contains then 3 records (user per room).

The test suite was written in JavaScript and is executed by Node.js inside a
Docker container. We picked JavaScript here due to the easy and good featured
[stanza.io](http://stanza.io) client library for XMPP. It got all the things
which were needed to fulfil the job.

## Additional readings

* [mod_mam MUC IQ integration](http://bit.ly/2M2cSWl)
* [mod_mam MUC message integration](http://bit.ly/2Kx69iF)
* [mod_muc implementation](http://bit.ly/2AJTSYq)
* [mod_muc_room implementation](http://bit.ly/2LX6As4)
* [mod_muc_room IQ implementation](http://bit.ly/2LWgXfI)
* [muc_filter_message hook example](http://bit.ly/2Oey9K0)
* [MUC message definition](http://bit.ly/2MavaVo)
* [MUCState definition](http://bit.ly/2AM4CWi)
* [XMPP codec API docs](http://bit.ly/2LXQ235)
* [XMPP codec guide](http://bit.ly/2LHKFoq)
* [XMPP codec script example](http://bit.ly/2M8sgNM)
