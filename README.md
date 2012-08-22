My personal take on haproxy support for Puppet.  Why not just use [glarizza's
module](https://github.com/glarizza/puppet-haproxy)?

 * Concat.  While it allows you to easily build complex config files quickly,
   it forces the user to manually order the fragments in the defined type
   declarations which is just user unfriendly.
 * Native types provide much easier to grok log output e.g.
   ```
   Node[foo]\Haproxy::backend[rails]\Server[server1]: maxconn changed from '16' to '32'
   ```

### Current status
Working on an Augeas lens for the haproxy.cfg format
