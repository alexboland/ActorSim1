import React, { useState, useEffect } from 'react';
import GovernmentUI from './government/GovernmentUI';
import HistoryExplorer from './HistoryExplorer';
import * as d3 from "d3"; // Imports all d3 modules

const App = () => {
  const [government, setGovernment] = useState(null);
  const [regions, setRegions] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [isHistoryModalOpen, setIsHistoryModalOpen] = useState(false);

  useEffect(() => {
    // Combined function to initialize government and fetch/seed regions
    const initializeApp = async () => {
      setLoading(true);
      try {
        // First initialize government
        await initializeGovernment();

        // Then check if regions exist and seed only if needed
        await fetchOrSeedRegions();
      } catch (error) {
        console.error('Failed to initialize app:', error);
        setError(error.message);
      } finally {
        setLoading(false);
      }
    };

    initializeApp();
  }, []); // Empty dependency array ensures this runs only once

  const initializeGovernment = async () => {
    try {
      // First, try to ping the existing government
      let response = await fetch('http://localhost:8080/government/ping');
      if (!response.ok) {
        // If ping fails, create a new government
        response = await fetch('http://localhost:8080/government/create', { method: 'POST' });
      }

      const data = await response.json();
      console.log("Received government data:", data);
      setGovernment(data);
    } catch (error) {
      console.error('Failed to initialize government:', error);
      throw error; // Propagate the error to be handled by the caller
    }
  };

  const fetchOrSeedRegions = async () => {
    try {
      // First, check if regions already exist
      const fetchRegionsResponse = await fetch('http://localhost:8080/regions');
      console.log("Fetch regions response:", fetchRegionsResponse);

      if (!fetchRegionsResponse.ok) {
        throw new Error(`Failed to fetch regions! Status: ${fetchRegionsResponse.status}`);
      }

      const fetchedRegions = await fetchRegionsResponse.json();
      console.log("Fetched regions:", fetchedRegions);

      // If regions exist, just use them
      if (fetchedRegions && Object.keys(fetchedRegions).length > 0) {
        console.log("Using existing regions");
        setRegions(fetchedRegions);
      } else {
        // Otherwise, generate new points and seed regions
        console.log("No regions found, seeding new regions");
        await seedRegions();
      }
    } catch (error) {
      console.error('Failed to fetch/seed regions:', error);
      throw error; // Propagate the error
    }
  };

  const seedRegions = async () => {
    // Set up dimensions and number of points
    const width = 7200;
    const height = 4200;
    const numPoints = 20; // Adjust the number of vertices as desired

    // Generate an array of random points: each point is an array [x, y]
    const points = d3.range(numPoints).map(() => ({
      x: Math.round(Math.random() * width),
      y: Math.round(Math.random() * height)
    }));

    console.log("Generated points for seeding:", points);

    try {
      // Seed regions with the generated points
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
      console.log("Successfully seeded regions:", seededRegions);

      // Handle the response - this depends on your API's response format
      if (seededRegions.uuids) {
        // If API returns array of UUIDs, convert to expected format
        const regionsMap = seededRegions.uuids.reduce((acc, uuid) => {
          acc[uuid] = { id: uuid, population: 100 };
          return acc;
        }, {});
        setRegions(regionsMap);
      } else {
        // Otherwise, assume the API returns the regions in the expected format
        setRegions(seededRegions);
      }
    } catch (error) {
      console.error('Failed to seed regions:', error);
      throw error;
    }
  };

  const openHistoryModal = () => {
    setIsHistoryModalOpen(true);
  };

  const closeHistoryModal = () => {
    setIsHistoryModalOpen(false);
  };

  if (loading) {
    return <div>Loading government data...</div>;
  }

  if (error) {
    return <div>Error: {error}</div>;
  }

  return (
    <div style={{ fontFamily: 'Arial, sans-serif', maxWidth: '1200px', margin: '0 auto', padding: '20px' }}>
      <header style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        marginBottom: '20px'
      }}>
        <h1>Region Simulator</h1>
        <button
          onClick={openHistoryModal}
          style={{
            padding: '10px 15px',
            backgroundColor: '#2196F3',
            color: 'white',
            border: 'none',
            borderRadius: '4px',
            cursor: 'pointer',
            fontSize: '16px'
          }}
        >
          History Explorer
        </button>
      </header>

      {government ? (
        <GovernmentUI initialGovernment={government} initialRegions={regions} />
      ) : (
        <div>No government data available.</div>
      )}

      <HistoryExplorer
        isOpen={isHistoryModalOpen}
        onClose={closeHistoryModal}
      />
    </div>
  );
};

export default App;