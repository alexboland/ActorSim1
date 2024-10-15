import React, { useState, useEffect } from 'react';
import FarmUI from './FarmUI';

const RegionDetailsUI = ({ regionId, onClose }) => {
  const [regionDetails, setRegionDetails] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchRegionDetails = async () => {
      try {
        const response = await fetch(`http://localhost:8080/regions/${regionId}/fullDetails`);
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const data = await response.json();
        setRegionDetails(data);
      } catch (error) {
        console.error(`Failed to fetch details for region ${regionId}:`, error);
      } finally {
        setLoading(false);
      }
    };

    fetchRegionDetails();
  }, [regionId]);

  const addFarm = async () => {
    try {
      const response = await fetch(`http://localhost:8080/regions/${regionId}/build/farm`, {
        method: 'POST',
      });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      // Refresh region details after adding a farm
      const updatedDetailsResponse = await fetch(`http://localhost:8080/regions/${regionId}/fullDetails`);
      const updatedDetails = await updatedDetailsResponse.json();
      setRegionDetails(updatedDetails);
    } catch (error) {
      console.error(`Failed to add farm to region ${regionId}:`, error);
    }
  };

  if (loading) {
    return <div>Loading region details...</div>;
  }

  const renderAgentCard = (agent) => {
    switch (agent.type) {
      case 'Farm':
        return <FarmUI key={agent.id} farm={agent} />;
      // Add cases for other agent types here
      default:
        return (
          <div key={agent.id} style={styles.defaultCard}>
            <h3>{agent.type}</h3>
            <p>Population: {agent.population}</p>
          </div>
        );
    }
  };

  return (
    <div style={styles.container}>
      <button onClick={onClose} style={styles.closeButton}>×</button>
      <h2 style={styles.title}>Detailed View: Region {regionId}</h2>

      <div style={styles.section}>
        <h3>Region Information</h3>
        <p>Population: {regionDetails.region.population}</p>
        <p>Season: {regionDetails.region.season}</p>
      </div>

      <div style={styles.section}>
        <h3>Resources</h3>
        <h4>Base Production:</h4>
        <ul>
          {Object.entries(regionDetails.region.baseProduction).map(([resource, amount]) => (
            <li key={resource}>{resource}: {amount}</li>
          ))}
        </ul>
        <h4>Stored Resources:</h4>
        <ul>
          {Object.entries(regionDetails.region.storedResources).map(([resource, amount]) => (
            <li key={resource}>{resource}: {amount}</li>
          ))}
        </ul>
      </div>

      <div style={styles.section}>
        <h3>Agents</h3>
        <div style={styles.agentGrid}>
          {regionDetails.agents.map((agent) => renderAgentCard(agent))}
        </div>
      </div>

      <div style={styles.section}>
        <h3>Add Sub-entities</h3>
        <button onClick={addFarm} style={styles.addButton}>Add Farm</button>
        {/* Add more buttons for other sub-entities here */}
      </div>
    </div>
  );
};

const styles = {
  container: {
    padding: '20px',
    backgroundColor: '#f0f4f8',
    borderRadius: '8px',
    boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
    position: 'relative',
    maxWidth: '800px',
    margin: '0 auto',
  },
  title: {
    fontSize: '1.5em',
    marginBottom: '15px',
  },
  closeButton: {
    position: 'absolute',
    top: '10px',
    right: '10px',
    background: 'none',
    border: 'none',
    fontSize: '24px',
    cursor: 'pointer',
    color: '#4a5568',
  },
  section: {
    marginBottom: '20px',
  },
  addButton: {
    padding: '8px 12px',
    background: '#4a5568',
    color: 'white',
    border: 'none',
    borderRadius: '4px',
    cursor: 'pointer',
    marginRight: '10px',
  },
  agentGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fill, minmax(200px, 1fr))',
    gap: '15px',
  },
  defaultCard: {
    backgroundColor: '#ffffff',
    borderRadius: '8px',
    padding: '15px',
    boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
  },
};

export default RegionDetailsUI;