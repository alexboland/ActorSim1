import React, { useState, useEffect } from 'react';
import RegionsMap from './RegionsMap';

const GovernmentUI = ({ initialGovernment, initialRegions }) => {
  const [government, setGovernment] = useState(null);
  const [regions, setRegions] = useState([]);
  const [foodPrice, setFoodPrice] = useState(1);
  const [selectedRegion, setSelectedRegion] = useState(null);

  // Effect to handle initial government setup
  useEffect(() => {
    if (initialGovernment) {
      setGovernment(initialGovernment);
    }
  }, [initialGovernment]);

  useEffect(() => {
    if (initialRegions) {
      setRegions(initialRegions);
    }
  }, [initialRegions]);

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

  const updateRegionInfo = (uuid, info) => {
    setRegions(prevRegions => ({
      ...prevRegions,
      [uuid]: { ...prevRegions[uuid], ...info }
    }));
  };

  const handleViewDetails = (regionId) => {
    setSelectedRegion(regionId);
  };

  const handleCloseDetails = () => {
    setSelectedRegion(null);
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
      </div>
      <div>

        <RegionsMap
          regions={regions}
          connections={[]}
        />
      </div>
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
  modalOverlay: {
    position: 'fixed',
    top: 0,
    left: 0,
    right: 0,
    bottom: 0,
    backgroundColor: 'rgba(0, 0, 0, 0.5)',
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
  },
  modalContent: {
    backgroundColor: 'white',
    padding: '20px',
    borderRadius: '8px',
    maxWidth: '80%',
    maxHeight: '80%',
    overflow: 'auto',
  },
};

export default GovernmentUI;