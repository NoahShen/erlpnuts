/*
Navicat MySQL Data Transfer

Source Server         : christy
Source Server Version : 50083
Source Host           : localhost:3306
Source Database       : test

Target Server Type    : MYSQL
Target Server Version : 50083
File Encoding         : 65001

Date: 2010-08-04 20:57:52
*/

SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `rev`
-- ----------------------------
DROP TABLE IF EXISTS `rev`;
CREATE TABLE `rev` (
  `tablename` varchar(50) NOT NULL,
  `fieldid` varchar(50) NOT NULL,
  `fieldname` varchar(50) default NULL,
  `fieldvalue` varchar(50) default NULL,
  `fieldversion` int(11) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- ----------------------------
-- Records of rev
-- ----------------------------
INSERT INTO `rev` VALUES ('User', 'Noah', 'name', 'Noah', '1');
INSERT INTO `rev` VALUES ('User', 'Noah', 'password', '123', '1');
INSERT INTO `rev` VALUES ('User', 'Noah', 'password', '1234', '2');
INSERT INTO `rev` VALUES ('User', 'Noah', 'password', '12345', '3');
INSERT INTO `rev` VALUES ('User', 'Noah', 'level', '2', '1');
INSERT INTO `rev` VALUES ('User', 'Noah', 'level', '3', '2');
