import React, { Component } from 'react';
import { LineChart, Line, CartesianGrid, XAxis, YAxis } from 'recharts';
import {popover, overlay} from './utils.js';

class PopulationChart extends Component {
  
  renderLineChart = (data) => (
    <LineChart width={600} height={300} data={data}>
      <Line type="monotone" dataKey="uv" stroke="#8884d8" />
      <CartesianGrid stroke="#ccc" />
      <XAxis dataKey="name" />
      <YAxis />
    </LineChart>
  );

  render() {
    const { data } = this.props;
    console.log(data);
    let dataFormatted = data.map(e => ({name: e.gen, uv: e.pop, pv: 2400, amt: 2400}))
    return (
      <div className="population-chart">
        <h3>Population chart</h3>
        { this.renderLineChart(dataFormatted) }
      </div>
    );
  }
}

export default PopulationChart;
