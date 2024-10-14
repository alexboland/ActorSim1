import React, { useState, useEffect } from 'react';

const RegionUI = ({ regionId, regionInfo, updateRegionInfo, onViewDetails }) => {
  const [isUpdating, setIsUpdating] = useState(false);

  const pingRegion = async () => {
    setIsUpdating(true);
    try {
      const response = await fetch(`http://localhost:8080/regions/${regionId}/ping`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      updateRegionInfo(regionId, data);
    } catch (error) {
      console.error(`Failed to ping region ${regionId}:`, error);
    } finally {
      setIsUpdating(false);
    }
  };

  useEffect(() => {
    if (!regionInfo.population) {
      pingRegion();
    }
  }, [regionId, regionInfo.population]);

  return (
    <div
      style={{
        ...styles.regionBox,
        ...(isUpdating ? styles.updating : {}),
      }}
    >
      <h3 style={styles.regionTitle}>Region: {regionId}</h3>
      <p style={styles.regionInfo}>Population: {regionInfo.population || 'Loading...'}</p>
      <p style={styles.regionInfo}>Season: {regionInfo.season || 'Loading...'}</p>
      <div style={styles.resourcesSection}>
        <h4 style={styles.resourcesTitle}>Base Production:</h4>
        <ul style={styles.resourcesList}>
          {regionInfo.baseProduction ? (
            Object.entries(regionInfo.baseProduction).map(([resource, amount]) => (
              <li key={resource} style={styles.resourceItem}>
                {resource}: {amount}
              </li>
            ))
          ) : (
            <li>Loading resources...</li>
          )}
        </ul>
        <h4 style={styles.resourcesTitle}>Stored Resources:</h4>
        <ul style={styles.resourcesList}>
          {regionInfo.storedResources ? (
            Object.entries(regionInfo.storedResources).map(([resource, amount]) => (
              <li key={resource} style={styles.resourceItem}>
                {resource}: {amount}
              </li>
            ))
          ) : (
            <li>Loading resources...</li>
          )}
        </ul>
      </div>
      <div style={styles.buttonContainer}>
        <button onClick={pingRegion} style={styles.button}>
          Update
        </button>
        <button onClick={() => onViewDetails(regionId)} style={styles.button}>
          View Details
        </button>
      </div>
    </div>
  );
};

const styles = {
  regionBox: {
    border: '1px solid #4a5568',
    borderRadius: '8px',
    padding: '15px',
    background: 'linear-gradient(to bottom right, #ffffff, #f0f0f0)',
    cursor: 'pointer',
    transition: 'all 0.3s ease',
    boxShadow: '0 2px 5px rgba(0,0,0,0.1)',
  },
  updating: {
    background: 'linear-gradient(to bottom right, #e9d5ff, #d8b4fe)',
    transform: 'scale(1.05)',
  },
  regionTitle: {
    fontSize: '1.2em',
    marginBottom: '10px',
    color: '#4a5568',
  },
  regionInfo: {
    fontSize: '1em',
    color: '#718096',
  },
  resourcesSection: {
    marginTop: '15px',
  },
  resourcesTitle: {
    fontSize: '1.1em',
    marginBottom: '5px',
    color: '#4a5568',
  },
  resourcesList: {
    listStyleType: 'none',
    padding: 0,
  },
  resourceItem: {
    fontSize: '0.9em',
    color: '#718096',
    marginBottom: '3px',
  },
  buttonContainer: {
    display: 'flex',
    justifyContent: 'space-between',
    marginTop: '15px',
  },
  button: {
    padding: '8px 12px',
    background: '#4a5568',
    color: 'white',
    border: 'none',
    borderRadius: '4px',
    cursor: 'pointer',
    transition: 'background 0.3s ease',
    '&:hover': {
      background: '#2d3748',
    },
  },
};

export default RegionUI;