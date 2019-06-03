import React, { Component } from 'react';
import Popover from 'react-bootstrap/Popover';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';

var popover = (title, text) => (
  <Popover id={title} title={title}>
    { text }
  </Popover>
)

 var overlay = (popover, child) => (
  <OverlayTrigger trigger="hover" placement="auto" overlay={popover}>
    { child }
  </OverlayTrigger>
)

export {
  overlay,
  popover
};
