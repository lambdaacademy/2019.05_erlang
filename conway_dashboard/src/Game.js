import React from 'react';
import './Game.css';
import Button from 'react-bootstrap/Button';
import ButtonToolbar from 'react-bootstrap/ButtonToolbar';
import Jumbotron from 'react-bootstrap/Jumbotron';
import Form from 'react-bootstrap/Form';
import ProgressBar from 'react-bootstrap/ProgressBar';
import Popover from 'react-bootstrap/Popover';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import ListGroup from 'react-bootstrap/ListGroup';
import Spinner from 'react-bootstrap/Spinner';
import PopulationChart from './population_chart.js';
import axios from 'axios';
import {popover, overlay} from './utils.js';


const SIZE = 800;
const defaultSideSize = 40;

class Cell extends React.Component {
  render() {
    const { x, y, size } = this.props;
    return (
      <div className="Cell" style={{
        left: `${size * x + 1}px`,
        top: `${size * y + 1}px`,
        width: `${size - 1}px`,
        height: `${size - 1}px`,
      }} />
    );
  }
}

class Game extends React.Component {


  constructor() {
    super();
    let cellSize = this.calcCellSize(defaultSideSize);
    this.board = this.makeEmptyBoard(defaultSideSize);

    this.state = {
      cells: [],
      socket: new WebSocket("ws://localhost:8080/websocket"),
      generation: null,
      goalGeneration: null,
      sideSize: defaultSideSize,
      chartData: [],
      locked: true,
      slaves: []
    }


    this.state.socket.onmessage = this.update_game_state
  }

  update_game_state = (event) => {
    var msg = JSON.parse(event.data);
    this.update_game(msg);
  }

  componentDidMount() {
    axios.get('http://localhost:8080/')
      .then(res => {
        this.update_game(res.data);
      })
      .catch(err => console.log(err))
  }

  update_game(state){
    console.log("Updating game state with:");
    console.log(state);
    
    let generation = null;
    let goalGeneration = null;
    let sideSize = defaultSideSize;
    let newCells = this.state.cells;

    let slaves = null;
    let maxSlaves = null;

    if(state.start_state === "computing") {
      generation = state.generation;
      goalGeneration = state.goalGeneration;
      sideSize = state.size;
      this.resize(sideSize);
      this.fill_board(state.fields);
      newCells = this.makeCells(sideSize);
    }
    if(state.start_state === "wait_for_slaves") {
      slaves = state.slaves;
      maxSlaves = state.max_slaves;
    }
    this.state.chartData.push({gen: generation, pop: newCells.length})
    this.setState({ 
      start_state: state.start_state,
      locked: (state.start_state === "computing" || state.start_state === "wait_for_slaves"),
      cells: newCells,
      generation: generation,
      sideSize: sideSize,
      goalGeneration: state.goalGeneration,
      slaves: state.slaves,
      maxSlaves: maxSlaves });
  }

  fill_board(fields) {
    fields.map(field => {this.board[field.x][field.y] = field.alive});
  }

  // Create an empty board
  makeEmptyBoard(size) {
    let board = [];
    for (let x = 0; x < size; x++) {
      board[x] = [];
      for (let y = 0; y < size; y++) {
        board[x][y] = false;
      }
    }
    return board;
  }
  // Create cells from this.board
  makeCells(size) {
    let cells = [];
    for (let x = 0; x < size; x++) {
      for (let y = 0; y < size; y++) {
        if (this.board[x][y]) {
          cells.push({ x, y });
        }
      }
    }
    return cells;
  }

  getElementOffset() {
    const rect = this.boardRef.getBoundingClientRect();
    console.log(rect);
    const doc = document.documentElement;
    return {
      x: rect.x,
      y: rect.y
    }
    return {
      x: (rect.left + window.pageXOffset) - doc.clientLeft,
      y: (rect.top + window.pageYOffset) - doc.clientTop,
    };
  }

  handleCellClick = (event) => {
    if(!this.state.locked) {
      const elemOffset = this.getElementOffset();
      console.log(elemOffset);
      const offsetX = event.clientX - elemOffset.x;
      console.log(event.clientY);
      const offsetY = event.clientY - elemOffset.y;
      const cellSize = this.calcCellSize(this.state.sideSize);

      const x = Math.floor(offsetX / cellSize);
      const y = Math.floor(offsetY / cellSize);
      if (x >= 0 && x <= this.state.sideSize && y >= 0 && y <= this.state.sideSize) {
        this.board[x][y] = !this.board[x][y];
      }
      this.setState({ cells: this.makeCells(this.state.sideSize) });
    }
  }

  handleStartClick = (event) => {
    event.preventDefault();
    this.setState({locked: true});
    let json = this.preparePayload(this.board, this.state.goalGeneration);
    axios.post('http://localhost:8080/', json)
      .then(function (response) {
        console.log(response);
      })
      .catch(function (error) {
        console.log(error);
      });
  }

  preparePayload(board, goalGenerations) {
    let fields = [];
    var w, k;
    for(w = 0; w < board.length; w++)
      for(k = 0; k < board[w].length; k++)
        fields.push({x: w, y: k, alive: board[w][k]});
    return {size: board.length, fields: fields, goalGenerations: parseInt(goalGenerations)};
  }

  handleGoalGenChange = (event) => {
    this.setState({ goalGeneration: event.target.value });
  }

  handleSizeChange = (event) => {
    let sideSize = event.target.value;
    this.resize(sideSize)
    this.setState({ sideSize: sideSize, cells: this.makeCells(sideSize)});
  }

  resize(newSize) {
    this.board = this.makeEmptyBoard(newSize)
  }

  calcCellSize = (sideSize) => SIZE / sideSize;

  boardDiv = () => {
    // sideSize is ** amount of cells on isde ** and SIZE is length in pixels
    const { cells, sideSize } = this.state;
    let cellSize = this.calcCellSize(sideSize);
    return (
      <div className="Board"
        style={{ width: SIZE, height: SIZE, backgroundSize: `${cellSize}px ${cellSize}px`}}
        onClick={this.handleCellClick}
        ref={(n) => { this.boardRef = n; }}>
        {cells.map(cell => (
          <Cell x={cell.x} y={cell.y} size={cellSize}
            key={`${cell.x},${cell.y}`}/>
        ))}
      </div>
    );
  }

  prestartForm = () => {
    if(this.state.start_state === "wait_for_start") {
      return (
        <div className="formDiv">
          <Form onSubmit={this.handleStartClick}>
            <Form.Group controlId="formBasicEmail">
              <Form.Label>Generations</Form.Label>
              <Form.Control className="genInput" type="number" placeholder="Enter generation number" onChange={this.handleGoalGenChange} />
              <Form.Label>Board size</Form.Label>
              <Form.Control className="board-size" type="number" placeholder="Enter board side size" onChange={this.handleSizeChange} />
            </Form.Group>
            <Button variant="primary" type="submit">
              Submit
            </Button>
          </Form>
        </div>
      )
    } else {
      return null;
    }
  }

  slavesProgress = () => {
    if(this.state.start_state) {
      let slaves = this.state.slaves;
      let slaves_n = slaves.length;
      let max_sl = this.state.maxSlaves;
      let progress = slaves_n / max_sl;

      const popoverr = popover("Slaves register progress", "This is a progress bar of registering slaves."); 
      // TODO style is a hack... why was it so small?
      const progressBar = (<ProgressBar style={{width: '2000%'}} animated now={progress} label={`${slaves_n}/${max_sl}`}/>);

      let slavesSpinner = null;
      if(max_sl != undefined && slaves_n != max_sl)
        slavesSpinner = (
          <div className="spinner-box">
            <Spinner animation="border" role="status">
              <span className="sr-only">Waiting for more slaves...</span>
            </Spinner>
          </div>
        );

      return (
          <div>
            {this.state.start_state === "waiting_for_slaves" && (
              <div className="slaves-list">
                <h3>Slaves progress bar</h3>
                { overlay(popoverr, progressBar) }
              </div>
            )}
            <h3>Registered slaves</h3>
            <ListGroup>
              { slaves.map(e => (<ListGroup.Item key={e}>{ e }</ListGroup.Item>)) }
              { slavesSpinner }
            </ListGroup>
          </div>
      );
    } else {
      return null;
    }
  }


  render() {
    let boardPopover = popover("Computation visualisation", "This is our board live. We can watch next generations in realtime. Click a cell to make it come to life.");
    let boardEl = this.boardDiv();
    let boardOverlay = overlay(boardPopover, boardEl);

    let progress = this.state.generation / this.state.goalGeneration;
    let progressBarOverlay = null;
    if(this.state.start_state === "computing"){
      const popoverr = popover("Generation progress", "This is a progress bar of our computations."); 
      const progressBar = (<ProgressBar animated now={progress} label={`${progress}%`}/>);
      progressBarOverlay = (
        <div>
          <h3>Progress bar</h3>
          { overlay(popoverr, progressBar) }
        </div>
      );
    }
    let prestartForm = this.prestartForm();

    let slavesProgressBar = this.slavesProgress();
    if(this.state.start_state === "finished") {
      // TODO test this branch!
      return (
        <Jumbotron>
          <h1>Conway game</h1>
          { boardOverlay }
          <div>
            <h2>Finished computation</h2>
            <PopulationChart data={ this.state.chartData }/>
          </div>
        </Jumbotron>
      )
    }
    return (
      <Jumbotron>
        <h1>Conway game</h1>
        { boardOverlay }
        <div className="dashboard">
          <div className="dashboard-title-div">
            <h2>Dashboard</h2>
          </div>
          <div className="dashboard-inside">
            { prestartForm }
            { slavesProgressBar }
            { progressBarOverlay }
            {this.state.start_state === "computing" && (
            <div className="statsDiv">
              {overlay(popover("Current generation", "This is the number of generations already calculated"), (<h3>Current generation</h3>))}
              { this.state.generation }
              {overlay(popover("Goal generations number", "This is the number of generations at which we will stop calculations"), (<h3>Goal generations</h3>))}
              { this.state.goalGeneration }
              <PopulationChart data={ this.state.chartData }/>
            </div>
            )}
          </div>
        </div>
      </Jumbotron>
    );
  }
}
export default Game;
