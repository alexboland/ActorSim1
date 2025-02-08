import React, { useState, useEffect } from 'react';
import GovernmentUI from './GovernmentUI';
import * as d3 from "d3"; // Imports all d3 modules

const App = () => {
  const [government, setGovernment] = useState(null);
  const [regions, setRegions] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {

    const seedRegions = async () => {
      // Set up dimensions and number of points.
      const width = 7200;
      const height = 4200;
      const numPoints = 50; // Adjust the number of vertices as desired

      // Generate an array of random points: each point is an array [x, y].
      const points = d3.range(numPoints).map(() => ({
        x: Math.round(Math.random() * width),
        y: Math.round(Math.random() * height)
      }));

      // Compute the Delaunay triangulation from the points.
      // D3’s Delaunay is dual to the Voronoi diagram.
      //const delaunay = d3.Delaunay.from(points);
      // Create a Voronoi diagram within our bounds.
      //const voronoi = delaunay.voronoi([0, 0, width, height]);

      // Select the SVG element and set its width and height.
      /*const svg = d3.select("#svg")
        .attr("width", width)
        .attr("height", height);*/

      const edges = [];
      /*points.forEach((p, i) => {
        // For each neighbor index j of point i...
        for (const j of voronoi.neighbors(i)) {
          // To avoid drawing duplicate edges, only add when i < j.
          if (i < j) {
            edges.push({ source: p, target: points[j] });
          }
        }
      });*/
          
      //TODO use list of edges to create roads

      console.log("points: ")
      console.log(points);

      try {
        console.log("==========")
        console.log(JSON.stringify({
          locations: points
        }))
        console.log("============")

        const fetchRegionsResponse = await fetch('http://localhost:8080/regions');
        console.log("fetch regions response");
        console.log(fetchRegionsResponse);
        const fetchedRegions = await fetchRegionsResponse.json();
        console.log("fetched regions");

        if (fetchedRegions.length === 0) {

          const response = await fetch('http://localhost:8080/regions/seed', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json'
            },
            body: JSON.stringify({
              locations: points
            })
          });

          if (!response.ok) {
            throw new Error(`Failed to seed regions! Status: ${response.status}`);
          }

          const seededRegions = await response.json();

          console.log("GETTING THIS FAR");


          console.log(seededRegions);
            
          // Convert array of regions to map and update state
          console.log("====MAP=====");
          console.log(seededRegions)
          const regionsMap = seededRegions.uuids.reduce((acc, uuid) => {
            acc[uuid] = {uuid, population: 100 };
            return acc;
          }, {});
          setRegions(regionsMap);
      } else {
        console.log("=====FETCHED REGIONS======");
        console.log(fetchedRegions);
        console.log("==========================")
        setRegions(fetchedRegions)
      }
        
      } catch (error) {
        console.error('Failed to seed regions:', error);
      }


    }

    const initializeGovernment = async () => {
      try {
        setLoading(true);
        // First, try to ping the existing government 
        let response = await fetch('http://localhost:8080/government/ping');
        if (!response.ok) {
          // If ping fails, create a new government
          // Eventually government will be structured differently but for now it's basically the "world"
          response = await fetch('http://localhost:8080/government/create', { method: 'POST' });
        }
 
        const data = await response.json();
        console.log("Received government data:", data); // Add this line for debugging

        setGovernment(data);
      } catch (error) {
        console.error('Failed to initialize government:', error);
        setError(error.message);
      } finally {
        setLoading(false);
      }
    };

    

    initializeGovernment();
    seedRegions();

  }, []);

  if (loading) {
    return <div>Loading government data...</div>;
  }

  if (error) {
    return <div>Error: {error}</div>;
  }

  return (
    <div style={{ fontFamily: 'Arial, sans-serif', maxWidth: '1200px', margin: '0 auto', padding: '20px' }}>
      <h1>Region Simulator</h1>
      {government ? (
        <GovernmentUI initialGovernment={government} initialRegions={regions} />
      ) : (
        <div>No government data available.</div>
      )}
    </div>
  );
};

export default App;