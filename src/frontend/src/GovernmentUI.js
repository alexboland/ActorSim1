import React, { useState, useEffect } from 'react';
import RegionUI from './RegionUI';

const GovernmentUI = ({ initialGovernment }) => {
  const [government, setGovernment] = useState(null);
  const [regions, setRegions] = useState({});
  const [foodPrice, setFoodPrice] = useState(1);

  useEffect(() => {
    if (initialGovernment && initialGovernment.regions) {
      setGovernment(initialGovernment);
      const initialRegions = initialGovernment.regions.reduce((acc, uuid) => {
        acc[uuid] = { uuid, population: null };
        return acc;
      }, {});
      setRegions(initialRegions);
    }
  }, [initialGovernment]);

  const updateFoodPrice = async () => {
    try {
      const response = await fetch('http://localhost:8080/government/food-price', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ price: foodPrice }),
      });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      // Optionally handle the response if the backend sends updated data
    } catch (error) {
      console.error('Failed to update food price:', error);
    }
  };

  const createRegion = async () => {
    try {
      const response = await fetch('http://localhost:8080/regions/create', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
      });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const newRegion = await response.json();

      setGovernment(prevGovernment => ({
        ...prevGovernment,
        regions: [...prevGovernment.regions, newRegion.uuid]
      }));

      setRegions(prevRegions => ({
        ...prevRegions,
        [newRegion.uuid]: { uuid: newRegion.uuid, population: null }
      }));
    } catch (error) {
      console.error('Failed to create new region:', error);
    }
  };

  const updateRegionInfo = (uuid, info) => {
    setRegions(prevRegions => ({
      ...prevRegions,
      [uuid]: { ...prevRegions[uuid], ...info }
    }));
  };

  if (!government) {
    return <div>Loading government data...</div>;
  }

  return (
    <div style={styles.governmentBox}>
      <h2 style={styles.heading}>Government Control Panel</h2>
      <div style={styles.controlPanel}>
        <input
          type="number"
          value={foodPrice}
          onChange={(e) => setFoodPrice(Number(e.target.value))}
          style={styles.input}
        />
        <button onClick={updateFoodPrice} style={styles.button}>
          Set Food Price
        </button>
        <button onClick={createRegion} style={styles.button}>
          Create New Region
        </button>
      </div>
      {government.regions.length > 0 ? (
        <div style={styles.regionGrid}>
          {government.regions.map((regionId) => (
            <RegionUI
              key={regionId}
              regionId={regionId}
              regionInfo={regions[regionId]}
              updateRegionInfo={updateRegionInfo}
            />
          ))}
        </div>
      ) : (
        <p>No regions available.</p>
      )}
    </div>
  );
};

const styles = {
  governmentBox: {
    border: '2px solid #4a5568',
    borderRadius: '12px',
    padding: '25px',
    marginBottom: '30px',
    background: 'linear-gradient(to bottom right, #e9d5ff, #8b5cf6)',
    boxShadow: '0 4px 6px rgba(0, 0, 0, 0.1)',
  },
  heading: {
    fontSize: '2em',
    color: '#ffffff',
    marginBottom: '20px',
  },
  controlPanel: {
    marginBottom: '25px',
    padding: '15px',
    background: 'rgba(255, 255, 255, 0.2)',
    borderRadius: '8px',
    display: 'flex',
    alignItems: 'center',
  },
  input: {
    marginRight: '15px',
    padding: '8px',
    borderRadius: '4px',
    border: '1px solid #4a5568',
  },
  button: {
    padding: '10px 15px',
    background: '#4a5568',
    color: 'white',
    border: 'none',
    borderRadius: '4px',
    cursor: 'pointer',
    marginRight: '15px',
    transition: 'background 0.3s ease',
    '&:hover': {
      background: '#2d3748',
    },
  },
  regionGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fill, minmax(250px, 1fr))',
    gap: '20px',
  },
};

export default GovernmentUI;